#' Create Partial Dependence Plot with Individual Conditional Expectation Lines
#'
#' This function creates partial dependence plots (PDP) with individual conditional
#' expectation (ICE) lines for ordinal random forest models. It automatically detects
#' whether the predictor variable is continuous or categorical and creates appropriate
#' visualizations. For continuous variables, it shows ICE lines with optional category
#' background mapping. For categorical variables, it creates ridgeline plots showing
#' the distribution of predictions for each category. test
#'
#' @param data A data frame containing the predictor variables and the response.
#' @param fit A fitted \code{ranger} model object.
#' @param response Character string. The name of the response variable in \code{data}.
#' @param x_var Character string specifying the name of the predictor variable to plot.
#' @param x_var_title Character string specifying x-axis title.
#' @param verbose Logical. If \code{TRUE}, returns a list with both data and plot.
#'  If \code{FALSE} (default), returns only the plot.
#' @param gridsize Integer. Number of points to evaluate in the grid for each variable. Default is 10.
#' @param nmax Integer. Maximum number of rows sampled from \code{data} for ICE computation. Default is 500.
#' @param nIce Integer or integer vector. If a single integer,
#' specifies the number of ICE curves to sample randomly (default is 30).
#' If a vector of integers, specifies the exact row indices of observations to use for ICE curves.
#' @param limits Optional numeric vector of y-axis limits. If \code{NULL}, computed from the data.
#' @param colorVar Optional variable name for colouring ICE curves (instead of prediction values).
#' @param probability Logical. Whether to use probability predictions. Default is \code{FALSE}.
#' @param borders Either a numeric vector of category borders or the string "auto" or "none".
#' If numeric vector, specifies the borders for category background mapping. If "auto" (default), uses
#' the model's category borders. If "none", no background mapping is shown.
#' Default is "auto".
#' @param category_colors Character vector of colors for category backgrounds.
#'  If NULL, uses default color palette.
#' @param category_alpha Numeric value between 0 and 1 for category background transparency.
#'  Default is 0.5.
#' @param category_names Character vector of category names for the legend.
#'  If NULL, uses default names.
#' @param category_title Character vector of the legend title.
#'  If NULL, no title is shown.
#' @param title Character string for the plot title. Default is "Partial Dependence
#'  Plot with Individual Conditional Expectation Lines".
#' @param subtitle Character string for the plot subtitle. Default is "Background
#'  colors show category mapping".
#' @param ice_alpha Numeric value between 0 and 1 for ICE line transparency.
#'  Default is 0.4.
#' @param ice_linecolor Character string specifying the color of ICE lines. Default is "purple".
#' @param ice_linewidth Numeric value for the width of ICE lines. Default is 0.7.
#' @param ice_pointsize Numeric value for the size of observed prodiction values. Default is 1.5.
#' @param pdp_linecolor Character string specifying the color of the PDP mean line.
#'  Default is "black".
#' @param pdp_linewidth Numeric value for the width of the PDP mean line. Default is 1.5.
#' @param pdp_intervalcolor Character string specifying the color of the interval lines for the PDP mean.
#'  Default is "black".
#' @param pdp_meancolor Character string specifying the color of the point's outline for the PDP mean.
#'  Default is "black".
#' @param pdp_meanfill Character string specifying the fill color of the point for the PDP mean.
#'  Default is "black".
#' @param pdp_meanshape Numeric value for the shape of the point for the PDP mean. Default is 16.
#' @param obs_color Character string for the color of observed prediction points. Default is "grey".
#' @param ridgeline_scale Numeric value controlling the height scaling of ridgeline
#'  plots for categorical variables. Default is 0.8.
#' @param show_vertical_lines Logical. If \code{TRUE}, adds vertical dashed lines at the category borders. Default is \code{TRUE}.
#' @param cond_color_var Character string specifying the name of a second predictor variable
#'  to use for conditional coloring of ICE curves. When specified, ICE curves are colored by this
#'  variable to explore potential interactions. Default is \code{NULL} (no conditional coloring).
#' @param cond_color_levels Character vector or named list specifying which levels/ranges to color.
#'  For categorical variables: character vector of factor levels (if NULL, uses first 3-5 levels).
#'  For continuous variables: named list where names are labels and values are numeric vectors of length 2
#'  specifying ranges, e.g., \code{list("Low" = c(0, 5), "High" = c(5, 10))}.
#' @param cond_color_palette Character vector of colors to use for conditional coloring.
#'  If NULL, uses a default color palette. Length should match number of conditioning levels/ranges.
#' @param cond_color_title Character string for the conditional color legend title.
#'  If NULL, uses the variable name from \code{cond_color_var}.
#' @param show_category_background Logical. If \code{TRUE}, shows colored background for category borders
#'  even when conditional coloring is active. Default is \code{FALSE} when \code{cond_color_var} is specified,
#'  \code{TRUE} otherwise.
#'
#' @return If \code{verbose = FALSE}, returns a ggplot object. If \code{verbose = TRUE},
#'  returns a list with elements:
#'  \item{plot}{The ggplot object}
#'  \item{data}{List containing ice_data and pdp_data tibbles}
#'  \item{variable_type}{Character indicating whether variable was treated as "categorical" or "continuous"}
#'
#' @details
#' The function automatically detects the variable type based on the original data:
#' \itemize{
#'  \item \strong{Categorical treatment}: Variables are treated as categorical if they are
#'   factors, characters, or have 4 or fewer unique values. A warning is displayed
#'   when numeric variables are treated as categorical due to having few unique values.
#'  \item \strong{Continuous variables}: Creates a traditional PDP with ICE lines,
#'   PDP mean line, and optional category background mapping using the \code{borders} parameter.
#'  \item \strong{Categorical variables}: Creates ridgeline plots showing the
#'   distribution of ICE predictions for each category level, with PDP mean points.
#'  \item \strong{Conditional ICE plots}: When \code{cond_color_var} is specified, ICE curves
#'   are colored by a second predictor variable to explore potential interactions (as recommended
#'   by Strobl et al. 2024). For continuous x-variables, ICE lines are colored by the conditioning
#'   variable. For categorical x-variables, separate density ridges are created for each combination
#'   of the x-variable and conditioning variable levels. By default, category background coloring
#'   is disabled when conditional coloring is active, showing only vertical lines at category borders.
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - just get the plot
#' plot <- plot_pdp(
#'  data = dm_train_dummy,
#'  model = rf_ord_dummy,
#'  response = "score",
#'  x_var = "item01",
#'  x_var_title = "Item 01"
#' )
#'
#' # Verbose usage - get plot, data, and variable type info
#' result <- plot_pdp(
#'  data = dm_train_dummy,
#'  model = rf_ord_dummy,
#'  response = "score",
#'  x_var = "item01",
#'  x_var_title = "Item 01",
#'  verbose = TRUE
#' )
#'
#' print(result$plot)
#' head(result$data$ice_data)
#'
#' # Conditional ICE plot with categorical conditioning variable
#' # Color ICE curves by gender to explore potential interactions
#' plot <- plot_pdp(
#'  data = dm_train_dummy,
#'  model = rf_ord_dummy,
#'  response = "score",
#'  x_var = "age",
#'  x_var_title = "Age",
#'  cond_color_var = "gender",
#'  cond_color_levels = c("Male", "Female"),  # Select specific levels
#'  cond_color_palette = c("#E69F00", "#56B4E9"),  # Custom colors
#'  cond_color_title = "Gender"
#' )
#'
#' # Conditional ICE plot with continuous conditioning variable
#' # Color ICE curves by income ranges
#' plot <- plot_pdp(
#'  data = dm_train_dummy,
#'  model = rf_ord_dummy,
#'  response = "score",
#'  x_var = "education",
#'  x_var_title = "Education Level",
#'  cond_color_var = "income",
#'  cond_color_levels = list(
#'    "Low" = c(0, 30000),
#'    "Medium" = c(30000, 70000),
#'    "High" = c(70000, 150000)
#'  ),
#'  cond_color_palette = c("#d73027", "#fee090", "#4575b4"),
#'  cond_color_title = "Income Range"
#' )
#'
#' # Conditional ICE plot with category background visible
#' plot <- plot_pdp(
#'  data = dm_train_dummy,
#'  model = rf_ord_dummy,
#'  response = "score",
#'  x_var = "age",
#'  x_var_title = "Age",
#'  cond_color_var = "gender",
#'  show_category_background = TRUE,  # Show colored background for categories
#'  borders = "auto"  # Use model's category borders
#' )
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import ggridges
#' @importFrom condvis2 CVpredict
#' @importFrom tibble as_tibble
#' @importFrom labeling rpretty
#' @importFrom rlang sym
#'
#' @export
plot_pdp <- function(
  data,
  model,
  response,
  x_var,
  x_var_title,
  verbose = FALSE,
  gridsize = 10,
  nmax = 500,
  nIce = 30,
  limits = NULL,
  colorVar = NULL,
  probability = FALSE,
  borders = "none",
  category_colors = NULL,
  category_alpha = 0.5,
  category_names = NULL,
  category_title = NULL,
  title = "Partial Dependence Plot",
  subtitle = "Background colors show category mapping",
  ice_alpha = 0.4,
  ice_linecolor = "lightgrey",
  ice_linewidth = 0.5,
  ice_pointsize = 3,
  pdp_linecolor = "#008080",
  pdp_linewidth = 0.5,
  pdp_intervalcolor = "black",
  pdp_meancolor = "#008080",
  pdp_meanfill = "black",
  pdp_meanshape = 23,
  obs_color = "black",
  obs_shape = 4,
  ridgeline_scale = 0.8,
  ridgeline_color = "lightgrey",
  show_vertical_lines = TRUE,
  seed = 42,
  cond_color_var = NULL,
  cond_color_levels = NULL,
  cond_color_palette = NULL,
  cond_color_title = NULL,
  show_category_background = NULL
) {
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  require(ggridges)
  require(condvis2)
  require(tibble)
  require(labeling)
  require(lme4)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Input validation

  if (!inherits(model, c("mixfabOF", "fabOF"))) {
    stop("Model is no 'mixfabOF' or 'fabOF' object.")
  }

  fit <- model$ranger.fit

  if (missing(x_var)) {
    stop("x_var must be specified")
  }

  if (!x_var %in% names(data)) {
    stop(paste0("Variable '", x_var, "' not found in data"))
  }

  # Create vector of variable names used in the model
  vars <- fit$forest$independent.variable.names

  # Add random effect name:
  random_terms <- lme4::findbars(model$random.formula)
  randeff_name <- as.character(random_terms[[1]][[3]])
  vars_with_randeff <- c(vars, randeff_name)

  for (var_name in vars) {
    #unique_values <- length(unique(data[[var_name]]))
    is_factor_or_char <- is.factor(data[[var_name]]) #|| is.character(data[[var_name]])
    #is_few_values <- unique_values <= 4

    is_categorical <- is_factor_or_char #|| is_few_values

    # Warning if treating numeric variable as categorical due to few values
    # if (!is_factor_or_char && is_few_values) {
    #   warning(paste0("Variable '", var_name, "' has only ", unique_values,
    #                  " unique values. Converting to factor."))
    # }
    #
    # # Transform to factor if categorical
    # if (is_categorical) {
    #   data[[var_name]] <- as.factor(data[[var_name]])
    # }
  }

  # Now check the specific plotting variable type AFTER transformation

  is_categorical <- is.factor(data[[x_var]])
  variable_type <- ifelse(is_categorical, "categorical", "continuous")

  # ============================================================================
  # CONDITIONAL COLORING SETUP
  # ============================================================================

  # Flag for whether conditional coloring is active
  use_conditional_coloring <- !is.null(cond_color_var)

  # Set default for show_category_background based on conditional coloring
  if (is.null(show_category_background)) {
    show_category_background <- !use_conditional_coloring
  }

  # If conditional coloring is requested, validate and process
  if (use_conditional_coloring) {
    # Validate that the conditioning variable exists in data
    if (!cond_color_var %in% names(data)) {
      stop(paste0("Conditioning variable '", cond_color_var, "' not found in data"))
    }

    # Check if conditioning variable is categorical or continuous
    cond_var_is_categorical <- is.factor(data[[cond_color_var]]) || is.character(data[[cond_color_var]])

    # Set default title for conditioning variable legend
    if (is.null(cond_color_title)) {
      cond_color_title <- cond_color_var
    }

    # Process categorical conditioning variable
    if (cond_var_is_categorical) {
      # Ensure it's a factor
      if (is.character(data[[cond_color_var]])) {
        data[[cond_color_var]] <- as.factor(data[[cond_color_var]])
      }

      # Get all levels
      all_levels <- levels(data[[cond_color_var]])

      # Determine which levels to color
      if (is.null(cond_color_levels)) {
        # Default: use first 3-5 levels (up to 5, or all if fewer)
        n_default_levels <- min(5, length(all_levels))
        cond_color_levels <- all_levels[1:n_default_levels]
        message(paste0("Using first ", n_default_levels, " levels of '", cond_color_var,
                      "' for conditional coloring: ", paste(cond_color_levels, collapse = ", ")))
      } else {
        # Validate user-specified levels
        invalid_levels <- setdiff(cond_color_levels, all_levels)
        if (length(invalid_levels) > 0) {
          stop(paste0("Invalid levels specified in cond_color_levels: ",
                     paste(invalid_levels, collapse = ", ")))
        }
      }

      # Set default color palette if not provided
      if (is.null(cond_color_palette)) {
        cond_color_palette <- scales::hue_pal()(length(cond_color_levels))
      } else if (length(cond_color_palette) != length(cond_color_levels)) {
        warning(paste0("cond_color_palette has ", length(cond_color_palette),
                      " colors but ", length(cond_color_levels), " levels specified. ",
                      "Adjusting palette."))
        cond_color_palette <- rep_len(cond_color_palette, length(cond_color_levels))
      }

    } else {
      # Process continuous conditioning variable
      if (is.null(cond_color_levels)) {
        stop("For continuous conditioning variables, cond_color_levels must be a named list specifying ranges, e.g., list('Low' = c(0, 5), 'High' = c(5, 10))")
      }

      # Validate that cond_color_levels is a named list
      if (!is.list(cond_color_levels) || is.null(names(cond_color_levels))) {
        stop("For continuous conditioning variables, cond_color_levels must be a named list")
      }

      # Validate that each element is a numeric vector of length 2
      for (i in seq_along(cond_color_levels)) {
        if (!is.numeric(cond_color_levels[[i]]) || length(cond_color_levels[[i]]) != 2) {
          stop(paste0("Each range in cond_color_levels must be a numeric vector of length 2. ",
                     "Problem with '", names(cond_color_levels)[i], "'"))
        }
        # Ensure ranges are ordered
        cond_color_levels[[i]] <- sort(cond_color_levels[[i]])
      }

      # Set default color palette if not provided
      if (is.null(cond_color_palette)) {
        cond_color_palette <- scales::hue_pal()(length(cond_color_levels))
      } else if (length(cond_color_palette) != length(cond_color_levels)) {
        warning(paste0("cond_color_palette has ", length(cond_color_palette),
                      " colors but ", length(cond_color_levels), " ranges specified. ",
                      "Adjusting palette."))
        cond_color_palette <- rep_len(cond_color_palette, length(cond_color_levels))
      }
    }
  }

  # ============================================================================
  # DATA GENERATION (from fun_pdp_data)
  # ============================================================================

  # Remove missing values
  data_clean <- na.omit(data)

  # Handle nmax sampling
  if (is.null(nmax)) {
    nmax <- nrow(data_clean)
  }
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data_clean)) {
    data_clean <- data_clean[sample(1:nrow(data_clean), nmax), , drop = FALSE]
  }

  # ICE curve sampling
  if (length(nIce) > 1) {
    nIce <- nIce[nIce <= nrow(data_clean)]
    sice <- c(NA, nIce)
  } else {
    nIce <- min(nIce, nrow(data_clean))
    sice <- c(NA, sample(1:nrow(data_clean), nIce))
  }

  # Create grid data for each variable
  pdplist1 <- vector(mode = "list", length = length(vars))
  # For each variable in vars split the range of the variable into gridsize many points.
  for (i in seq_along(vars)) {
    #creates data for each variable in vars, for each observation, for
    #for each gridpoint.
    #note: id refers to observation, pid refers to variable
    px <- vivid:::pdp_data(d = data_clean, var = vars[i], gridsize = gridsize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- dplyr::bind_rows(pdplist1)

  # Predict Y on this grid for each observation and variable

  pdplist1$fit <- predict(object = model, newdata = pdplist1, type = "latent")

  # Split by variable for easier handling
  pdplist1 <- base::split(x = pdplist1, f = pdplist1$.pid)
  names(pdplist1) <- vars

  # Extract data for the specified x_var
  var_index <- which(vars == x_var)
  if (length(var_index) == 0) {
    stop(paste0("Variable '", x_var, "' not found in processed variables"))
  }

  pdp <- pdplist1[[var_index]]

  # Aggregate mean PDP
  aggr <- pdp %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::summarise(fit = mean(fit))

  # Filter ICE data for selected curves
  ice_data_sample <- dplyr::filter(pdp, .data[[".id"]] %in% sice)

  # Here run a seperate prediction with the actually observed data to get the
  # observed prediction values for the sampled ICE curves:

  # Get the original data for the sampled ICE curves
  observed_data <- data_clean[sice[-1], , drop = FALSE] # Remove NA from sice
  observed_data$.id <- sice[-1] # Add .id column to match ice_data_sample

  # Predict on observed data
  observed_data$fit <- suppressWarnings(predict(
    object = model,
    newdata = observed_data,
    type = "latent"
  ))

  # Keep only the relevant columns for merging
  observed_predictions <- observed_data %>%
    select(.id, !!sym(x_var), fit) %>%
    rename(observed_fit = fit, observed_x = !!sym(x_var))

  # Merge observed predictions with ICE data sample
  ice_data_sample <- ice_data_sample %>%
    left_join(observed_predictions, by = ".id")

  # ============================================================================
  # ASSIGN CONDITIONAL COLORS TO ICE DATA
  # ============================================================================

  if (use_conditional_coloring) {
    # Add conditioning variable values to ice_data and ice_data_sample
    # by joining with the original data via .id

    # Create a lookup table for conditioning variable values
    cond_lookup <- data_clean %>%
      mutate(.id = row_number()) %>%
      select(.id, !!sym(cond_color_var))

    # Join conditioning variable to ice_data and ice_data_sample
    ice_data <- ice_data %>%
      left_join(cond_lookup, by = ".id")

    ice_data_sample <- ice_data_sample %>%
      left_join(cond_lookup, by = ".id")

    # Create color grouping variable
    if (cond_var_is_categorical) {
      # For categorical: filter to selected levels and create color group
      ice_data <- ice_data %>%
        filter(!!sym(cond_color_var) %in% cond_color_levels) %>%
        mutate(cond_color_group = factor(!!sym(cond_color_var), levels = cond_color_levels))

      ice_data_sample <- ice_data_sample %>%
        filter(!!sym(cond_color_var) %in% cond_color_levels) %>%
        mutate(cond_color_group = factor(!!sym(cond_color_var), levels = cond_color_levels))

    } else {
      # For continuous: assign to ranges
      cond_var_values <- ice_data[[cond_color_var]]

      # Create a function to assign range labels
      assign_range <- function(value, ranges) {
        for (i in seq_along(ranges)) {
          range_vals <- ranges[[i]]
          if (value >= range_vals[1] && value <= range_vals[2]) {
            return(names(ranges)[i])
          }
        }
        return(NA_character_)
      }

      # Assign range labels to each observation
      ice_data$cond_color_group <- sapply(ice_data[[cond_color_var]],
                                          assign_range,
                                          ranges = cond_color_levels)
      ice_data$cond_color_group <- factor(ice_data$cond_color_group,
                                          levels = names(cond_color_levels))

      ice_data_sample$cond_color_group <- sapply(ice_data_sample[[cond_color_var]],
                                                  assign_range,
                                                  ranges = cond_color_levels)
      ice_data_sample$cond_color_group <- factor(ice_data_sample$cond_color_group,
                                                  levels = names(cond_color_levels))

      # Filter out observations not in any range
      ice_data <- ice_data %>% filter(!is.na(cond_color_group))
      ice_data_sample <- ice_data_sample %>% filter(!is.na(cond_color_group))
    }

    # Create named color vector for use in plotting
    cond_color_scale <- setNames(cond_color_palette,
                                  if(cond_var_is_categorical) cond_color_levels else names(cond_color_levels))
  }

  # Create data list
  pdp_data_list <- list(
    ice_data = tibble::as_tibble(ice_data),
    ice_data_sample = ice_data_sample,
    pdp_data = tibble::as_tibble(aggr)
  )

  # ============================================================================
  # PLOTTING (from fun_pdp_plot)
  # ============================================================================

  # Extract data from list
  ice_data <- pdp_data_list$ice_data
  ice_data_sample <- pdp_data_list$ice_data_sample
  pdp_data <- pdp_data_list$pdp_data

  # Validate and set the 'borders' variable
  if (is.character(borders)) {
    if (borders == "auto") {
      borders <- model$category.borders
    } else if (borders == "none") {
      # If borders is 'none', we handle this by setting it to NULL or a similar flag
      # so that the plotting code knows not to add background rectangles.
      borders <- NULL
    } else {
      stop("If 'borders' is a character, it must be 'auto' or 'none'.")
    }
  } else if (!is.numeric(borders) || length(borders) < 2) {
    # If it's not a character, it must be a numeric vector of length 2 or more.
    stop(
      "'borders' must be 'auto', 'none', or a numeric vector with at least two values."
    )
  }

  # Create color palette if not provided
  if (is.null(category_colors) && is.numeric(borders)) {
    # colors <- c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C')
    # color_palette <- colorRampPalette(colors)
    # category_colors <- color_palette(length(borders) - 1)
    category_colors <- paletteer::paletteer_d(
      "PNWColors::Bay",
      n = length(borders) - 1
    )
  }

  # Get limits for y-axis
  if (is.null(limits)) {
    limits <- range(labeling::rpretty(
      min(c(ice_data_sample$fit, pdp_data$fit)),
      max(c(ice_data_sample$fit, pdp_data$fit))
    ))
  }

  # Get x-axis range for background rectangles
  if (is_categorical) {
    # For categorical variables, create range based on factor levels
    if (is.factor(ice_data_sample[[x_var]])) {
      x_range <- c(0.5, length(levels(ice_data_sample[[x_var]])) + 0.5)
    } else {
      x_range <- c(0.5, unique_values + 0.5)
    }
  } else {
    x_range <- range(ice_data_sample[[x_var]], na.rm = TRUE)
  }

  #browser()

  if (is.null(category_names) && is.numeric(borders)) {
    category_names <- paste0("cat", seq_len(length(borders) - 1))
  }

  # Create background rectangles for cont x_var if borders are provided
  # Only show background if show_category_background is TRUE
  if (!is_categorical && is.numeric(borders) && show_category_background) {
    # Function to create background rectangles
    create_background_rects <- function(borders, x_range) {
      rects <- data.frame()

      for (i in 1:(length(borders) - 1)) {
        ymin <- borders[i]
        ymax <- borders[i + 1]

        # Handle -Inf and Inf
        if (is.infinite(ymin)) {
          ymin <- limits[1]
        }
        if (is.infinite(ymax)) {
          ymax <- limits[2]
        }

        rects <- rbind(
          rects,
          data.frame(
            xmin = x_range[1],
            xmax = x_range[2],
            ymin = ymin,
            ymax = ymax,
            category = i
          )
        )
      }
      return(rects)
    }

    bg_rects <- create_background_rects(borders, x_range)

    # Create the base plot with background
    p <- ggplot() +
      # Add background rectangles for categories
      geom_rect(
        data = bg_rects,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = factor(category)
        ),
        alpha = category_alpha
      ) +
      # Customize fill colors for background
      scale_fill_manual(
        values = category_colors[1:length(unique(bg_rects$category))],
        name = category_title,
        labels = category_names[1:length(unique(bg_rects$category))]
      ) +
      guides(fill = guide_legend(reverse = TRUE))

    if (show_vertical_lines && is.numeric(borders)) {
      # Add horizontal lines for borders (excluding -Inf and Inf)
      p <- p +
        geom_hline(
          yintercept = borders[!is.infinite(borders)],
          color = "black",
          linetype = "dashed",
          alpha = 0.8
        )
    }
  } else {
    # Create base plot without background
    p <- ggplot()
  }

  #browser()

  if (is_categorical) {
    # Convert factor to numeric while preserving original factor levels for labeling
    factor_levels <- levels(ice_data_sample[[x_var]])
    factor_labels <- factor_levels # Store original labels

    # Convert factor to numeric in the data
    ice_data_sample_numeric <- ice_data_sample %>%
      mutate(!!sym(x_var) := as.numeric(!!sym(x_var)))

    ice_data_numeric <- ice_data %>%
      mutate(!!sym(x_var) := as.numeric(!!sym(x_var)))

    # Prepare observed data points - only the actual observed values
    observed_points <- ice_data_sample_numeric %>%
      filter(!is.na(observed_fit)) %>%
      mutate(
        x_value_numeric = as.numeric(!!sym(x_var)),
        observed_x_numeric = as.numeric(observed_x)
      ) %>%
      # Only keep points where the x_var matches the observed_x
      filter(x_value_numeric == observed_x_numeric) %>%
      select(x_value = x_value_numeric, observed_fit, .id)

    # CATEGORICAL PLOT with ridgelines (now using numeric values)

    if (use_conditional_coloring) {
      # For conditional coloring: create separate densities per combination
      # Prepare data with combination groups
      ridgeline_data <- ice_data_sample_numeric %>%
        select(all_of(x_var), fit, .id, cond_color_group) %>%
        rename(x_value = !!sym(x_var)) %>%
        filter(!is.na(cond_color_group))

      # Create y-position for each combination
      # For each x_var level, we'll have multiple ridges (one per cond_color_group)
      n_cond_levels <- length(levels(ridgeline_data$cond_color_group))

      # Create combined grouping variable for ridgelines
      ridgeline_data <- ridgeline_data %>%
        mutate(
          # Create a combined y-position that separates densities
          y_position = x_value + (as.numeric(cond_color_group) - 1) * 0.15 -
                       (n_cond_levels - 1) * 0.15 / 2
        )

      # Create labels for y-axis (only show x_var labels at main positions)
      y_breaks <- seq_along(factor_levels)
      y_labels <- factor_labels

      p <- ggplot(data = ridgeline_data) +
        stat_density_ridges(
          mapping = aes(
            y = y_position,
            group = interaction(x_value, cond_color_group),
            x = fit,
            fill = cond_color_group
          ),
          geom = "density_ridges",
          scale = ridgeline_scale / n_cond_levels,
          rel_min_height = 0,
          color = ridgeline_color,
          alpha = 0.7
        ) +
        # Add conditional color scale
        scale_fill_manual(
          values = cond_color_scale,
          name = cond_color_title,
          na.translate = FALSE
        ) +
        # Add scale_y_continuous to restore original factor labels
        scale_y_continuous(
          breaks = y_breaks,
          labels = y_labels,
          name = x_var_title
        ) +
        # Labels and theme
        labs(
          x = "Latent Score",
          title = title,
          subtitle = subtitle
        ) +
        theme_minimal() +
        coord_flip() +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5)
        )

      # Optionally add category borders as background or vertical lines
      if (is.numeric(borders) && show_category_background) {
        # Note: background coloring with conditional coloring is complex
        # For now, we'll just add vertical lines
        p <- p +
          geom_vline(
            xintercept = borders[!is.infinite(borders)],
            color = "black",
            linetype = "dashed",
            alpha = 0.8
          )
      } else if (show_vertical_lines && is.numeric(borders)) {
        p <- p +
          geom_vline(
            xintercept = borders[!is.infinite(borders)],
            color = "black",
            linetype = "dashed",
            alpha = 0.8
          )
      }

    } else {
      # Original non-conditional plot
      # Prepare data for ridgelines - group ICE curves by x_var value
      ridgeline_data <- ice_data_sample_numeric %>%
        select(all_of(x_var), fit, .id) %>%
        rename(x_value = !!sym(x_var))

      p <- ggplot(data = ridgeline_data) +
        stat_density_ridges(
          mapping = aes(
            y = .data$x_value,
            group = .data$x_value,
            x = fit,
            fill = after_stat(cut(x, breaks = borders))
          ),
          geom = "density_ridges_gradient",
          scale = ridgeline_scale,
          rel_min_height = 0,
          color = ridgeline_color,
          jittered_points = FALSE,
          point_color = obs_color,
          point_shape = obs_shape,
          point_size = ice_pointsize,
          point_alpha = ice_alpha
        ) +
        # Add observed prediction points
        geom_point(
          data = observed_points,
          aes(x = observed_fit, y = x_value),
          color = obs_color,
          shape = obs_shape,
          size = ice_pointsize,
          alpha = 1 # Full opacity for observed points
        ) +
        # Add line connecting the mean values
        geom_line(
          data = ice_data_numeric %>%
            select(all_of(x_var), fit, .id) %>%
            rename(x_value = !!sym(x_var)) %>%
            group_by(x_value) %>%
            summarise(mean_fit = mean(fit), .groups = "drop"),
          mapping = aes(y = x_value, x = mean_fit),
          color = pdp_meancolor, # Use same color as the mean points
          size = pdp_linewidth,
          position = position_nudge(y = -0.05) # Same nudge as the points
        ) +
        ggdist::stat_pointinterval(
          data = ice_data_numeric %>%
            select(all_of(x_var), fit, .id) %>%
            rename(x_value = !!sym(x_var)),
          point_interval = "mean_qi",
          mapping = aes(y = .data$x_value, group = .data$x_value, x = fit),
          position = position_nudge(y = -0.05),
          color = obs_color,
          shape = pdp_meanshape,
          interval_color = pdp_intervalcolor,
          point_color = pdp_meancolor,
          point_fill = pdp_meanfill
        ) +
        # Add scale_y_continuous to restore original factor labels
        scale_y_continuous(
          breaks = seq_along(factor_levels),
          labels = factor_labels,
          name = x_var_title
        ) +
        guides(
          fill = guide_legend(
            reverse = TRUE,
            # Override the point aesthetics in the legend
            override.aes = list(
              point_color = NA,
              point_size = NA,
              point_alpha = NA
            )
          )
        ) +
        # Labels and theme
        labs(
          x = "Latent Score",
          title = title,
          subtitle = subtitle
        ) +
        theme_minimal() +
        coord_flip() +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5)
        )

      if (is.numeric(borders)) {
        p <- p +
          scale_fill_manual(
            values = scales::alpha(
              category_colors[1:(length(borders) - 1)],
              alpha = category_alpha
            ),
            name = category_title,
            labels = category_names[1:(length(borders) - 1)]
          )
      }

      if (show_vertical_lines && is.numeric(borders)) {
        p <- p +
          geom_vline(
            xintercept = borders[!is.infinite(borders)],
            color = "black",
            linetype = "dashed",
            alpha = 0.8
          )
      }
    }
  } else {
    # CONTINUOUS PLOT with ICE lines and PDP
    #browser()
    # Add continuous-specific elements to the plot

    if (use_conditional_coloring) {
      # Add ICE lines with conditional coloring
      p <- p +
        geom_line(
          data = ice_data_sample,
          aes(x = !!sym(x_var), y = fit, group = .id, color = cond_color_group),
          alpha = ice_alpha,
          linewidth = ice_linewidth
        ) +
        # Add conditional color scale
        scale_color_manual(
          values = cond_color_scale,
          name = cond_color_title,
          na.translate = FALSE
        )
    } else {
      # Add ICE lines without conditional coloring
      p <- p +
        geom_line(
          data = ice_data_sample,
          aes(x = !!sym(x_var), y = fit, group = .id),
          color = ice_linecolor,
          alpha = ice_alpha,
          linewidth = ice_linewidth
        )
    }

    # Add PDP mean line
    p <- p +
      geom_line(
        data = pdp_data,
        aes(x = !!sym(x_var), y = fit),
        color = pdp_linecolor,
        linewidth = pdp_linewidth
      ) +

      # Add observed predictions as points
      geom_point(
        data = ice_data_sample %>%
          filter(!is.na(observed_fit)), # Only plot where observed data exists
        aes(x = observed_x, y = observed_fit),
        color = obs_color,
        shape = obs_shape,
        size = ice_pointsize,
        alpha = 1 # Full opacity for observed points to make them stand out
      ) +

      # Set axis limits
      coord_cartesian(ylim = limits) +

      # Labels and theme
      labs(
        x = x_var_title,
        y = "Latent Score",
        title = title,
        subtitle = subtitle
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "white", linewidth = 0.5),
        panel.grid.major.y = element_line(color = "white", linewidth = 0.5)
      )
  }

  # Return based on verbose setting
  if (verbose) {
    return(list(
      plot = p,
      data = pdp_data_list,
      variable_type = variable_type
    ))
  } else {
    return(p)
  }
}
