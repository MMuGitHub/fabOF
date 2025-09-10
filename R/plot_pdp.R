#' Create Partial Dependence Plot with Individual Conditional Expectation Lines
#'
#' This function creates partial dependence plots (PDP) with individual conditional 
#' expectation (ICE) lines for ordinal random forest models. It automatically detects 
#' whether the predictor variable is continuous or categorical and creates appropriate 
#' visualizations. For continuous variables, it shows ICE lines with optional category 
#' background mapping. For categorical variables, it creates ridgeline plots showing 
#' the distribution of predictions for each category.
#'
#' @param data A data frame containing the predictor variables and the response.
#' @param fit A fitted \code{ranger} model object.
#' @param response Character string. The name of the response variable in \code{data}.
#' @param x_var Character string specifying the name of the predictor variable to plot.
#' @param verbose Logical. If \code{TRUE}, returns a list with both data and plot. 
#'   If \code{FALSE} (default), returns only the plot.
#' @param gridSize Integer. Number of points to evaluate in the grid for each variable. Default is 10.
#' @param nmax Integer. Maximum number of rows sampled from \code{data} for ICE computation. Default is 500.
#' @param nIce Integer. Number of ICE curves to sample. Default is 30.
#' @param limits Optional numeric vector of y-axis limits. If \code{NULL}, computed from the data.
#' @param colorVar Optional variable name for colouring ICE curves (instead of prediction values).
#' @param probability Logical. Whether to use probability predictions. Default is \code{FALSE}.
#' @param borders Numeric vector of category borders for continuous variables. 
#'   If NULL, no background category mapping is shown. Default is NULL.
#' @param category_colors Character vector of colors for category backgrounds. 
#'   If NULL, uses default color palette.
#' @param category_alpha Numeric value between 0 and 1 for category background transparency. 
#'   Default is 0.5.
#' @param category_names Character vector of category names for the legend. 
#'   If NULL, uses default names.
#' @param title Character string for the plot title. Default is "Partial Dependence 
#'   Plot with Individual Conditional Expectation Lines".
#' @param subtitle Character string for the plot subtitle. Default is "Background 
#'   colors show category mapping".
#' @param ice_alpha Numeric value between 0 and 1 for ICE line transparency. 
#'   Default is 0.4.
#' @param ice_color Character string specifying the color of ICE lines. Default is "purple".
#' @param ice_linewidth Numeric value for the width of ICE lines. Default is 0.7.
#' @param pdp_color Character string specifying the color of the PDP mean line and points. 
#'   Default is "black".
#' @param pdp_linewidth Numeric value for the width of the PDP mean line. Default is 1.5.
#' @param ridgeline_scale Numeric value controlling the height scaling of ridgeline 
#'   plots for categorical variables. Default is 0.8.
#' @param ridgeline_alpha Numeric value between 0 and 1 for ridgeline transparency 
#'   in categorical plots. Default is 0.7.
#'
#' @return If \code{verbose = FALSE}, returns a ggplot object. If \code{verbose = TRUE}, 
#'   returns a list with elements:
#'   \item{plot}{The ggplot object}
#'   \item{data}{List containing ice_data and pdp_data tibbles}
#'   \item{variable_type}{Character indicating whether variable was treated as "categorical" or "continuous"}
#'
#' @details
#' The function automatically detects the variable type based on the original data:
#' \itemize{
#'   \item \strong{Categorical treatment}: Variables are treated as categorical if they are 
#'     factors, characters, or have 4 or fewer unique values. A warning is displayed 
#'     when numeric variables are treated as categorical due to having few unique values.
#'   \item \strong{Continuous variables}: Creates a traditional PDP with ICE lines, 
#'     PDP mean line, and optional category background mapping using the \code{borders} parameter.
#'   \item \strong{Categorical variables}: Creates ridgeline plots showing the 
#'     distribution of ICE predictions for each category level, with PDP mean points.
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - just get the plot
#' plot <- plot_pdp(
#'   data = dm_train_dummy, 
#'   fit = rf_ord_dummy$ranger.fit,
#'   response = "score",
#'   var = "item01"
#' )
#' 
#' # Verbose usage - get plot, data, and variable type info
#' result <- plot_pdp(
#'   data = dm_train_dummy, 
#'   fit = rf_ord_dummy$ranger.fit,
#'   response = "score",
#'   var = "item01",
#'   verbose = TRUE
#' )
#' 
#' print(result$plot)
#' head(result$data$ice_data)
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
plot_pdp <- function(data,
                     model,
                     response,
                     x_var,
                     verbose = FALSE,
                     gridSize = 10,
                     nmax = 500,
                     nIce = 30,
                     limits = NULL,
                     colorVar = NULL,
                     probability = FALSE,
                     borders = NULL,
                     category_colors = NULL,
                     category_alpha = 0.5,
                     category_names = NULL,
                     title = "Partial Dependence Plot with Individual Conditional Expectation Lines",
                     subtitle = "Background colors show category mapping",
                     ice_alpha = 0.4,
                     ice_color = "purple",
                     ice_linewidth = 0.7,
                     pdp_color = "black",
                     pdp_linewidth = 1.5,
                     ridgeline_scale = 0.8,
                     ridgeline_alpha = 0.7) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  require(ggridges)
  require(condvis2)
  require(tibble)
  require(labeling)
  require(lme4)
  
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
    unique_values <- length(unique(data[[var_name]]))
    is_factor_or_char <- is.factor(data[[var_name]]) || is.character(data[[var_name]])
    is_few_values <- unique_values <= 4
    
    is_categorical <- is_factor_or_char || is_few_values
    
    # Warning if treating numeric variable as categorical due to few values
    if (!is_factor_or_char && is_few_values) {
      warning(paste0("Variable '", var_name, "' has only ", unique_values, 
                     " unique values. Converting to factor."))
    }
    
    # Transform to factor if categorical
    if (is_categorical) {
      data[[var_name]] <- as.factor(data[[var_name]])
    }
  }
  
  # Now check the specific plotting variable type AFTER transformation
  
  is_categorical <- is.factor(data[[x_var]])
  variable_type <- ifelse(is_categorical, "categorical", "continuous")
  
  # ============================================================================
  # DATA GENERATION (from fun_pdp_data)
  # ============================================================================
  
  # Remove missing values
  data_clean <- na.omit(data)
  
  # Handle nmax sampling
  if (is.null(nmax)) nmax <- nrow(data_clean)
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
  # For each variable in vars split the range of the variable into gridSize many points.
  for (i in seq_along(vars)) {
    #creates data for each variable in vars, for each observation, for 
    #for each gridpoint.
    #note: id refers to observation, pid refers to variable
    px <- vivid:::pdp_data(d = data_clean, var = vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- dplyr::bind_rows(pdplist1)
  
  # Predict Y on this grid for each observation and variable
  browser()
  pdplist1$fit <- predict(object = model, newdata = pdplist1)

  
  # Split by variable for easier handling
  pdplist1 <- split(pdplist1, pdplist1$.pid)
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
    dplyr::summarise(fit = mean(fit), .groups = "drop")
  
  # Filter ICE data for selected curves
  pdp1 <- dplyr::filter(pdp, .data[[".id"]] %in% sice)
  
  # Create data list
  pdp_data_list <- list(
    ice_data = tibble::as_tibble(pdp1),
    pdp_data = tibble::as_tibble(aggr)
  )
  
  # ============================================================================
  # PLOTTING (from fun_pdp_plot)
  # ============================================================================
  
  # Extract data from list
  ice_data <- pdp_data_list$ice_data
  pdp_data <- pdp_data_list$pdp_data
  
  # Create color palette if not provided
  if (is.null(category_colors) && !is.null(borders)) {
    colors <- c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C')
    color_palette <- colorRampPalette(colors)
    category_colors <- color_palette(length(borders) - 1)
  }
  
  # Get limits for y-axis
  if (is.null(limits)) {
    limits <- range(labeling::rpretty(
      min(c(ice_data$fit, pdp_data$fit)),
      max(c(ice_data$fit, pdp_data$fit))
    ))
  }
  
  # Get x-axis range for background rectangles
  if (is_categorical) {
    # For categorical variables, create range based on factor levels
    if (is.factor(ice_data[[x_var]])) {
      x_range <- c(0.5, length(levels(ice_data[[x_var]])) + 0.5)
    } else {
      x_range <- c(0.5, unique_values + 0.5)
    }
  } else {
    x_range <- range(ice_data[[x_var]], na.rm = TRUE)
  }
  
  # Create background rectangles if borders are provided
  if (!is.null(borders)) {
    
    if (is.null(category_names)) {
      category_names <- paste0("cat", seq_len(length(borders) - 1))
    }
    
    # Function to create background rectangles
    create_background_rects <- function(borders, x_range) {
      rects <- data.frame()
      
      for (i in 1:(length(borders) - 1)) {
        ymin <- borders[i]
        ymax <- borders[i + 1]
        
        # Handle -Inf and Inf
        if (is.infinite(ymin)) ymin <- limits[1]
        if (is.infinite(ymax)) ymax <- limits[2]
        
        rects <- rbind(rects, data.frame(
          xmin = x_range[1],
          xmax = x_range[2], 
          ymin = ymin,
          ymax = ymax,
          category = i
        ))
      }
      return(rects)
    }
    
    bg_rects <- create_background_rects(borders, x_range)
    
    # Create the base plot with background
    p <- ggplot() +
      # Add background rectangles for categories
      geom_rect(
        data = bg_rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            fill = factor(category)),
        alpha = category_alpha
      ) +
      
      # Add horizontal lines for borders (excluding -Inf and Inf)
      geom_hline(
        yintercept = borders[!is.infinite(borders)],
        color = "black", 
        linetype = "dashed", 
        alpha = 0.8
      ) +
      
      # Customize fill colors for background
      scale_fill_manual(
        values = category_colors[1:length(unique(bg_rects$category))],
        name = "Rating",
        labels = category_names[1:length(unique(bg_rects$category))]
      ) +
      guides(fill = guide_legend(reverse = TRUE))
    
  } else {
    # Create base plot without background
    p <- ggplot()
  }
  
  if (is_categorical) {
    # CATEGORICAL PLOT with ridgelines
    
    # Prepare data for ridgelines - group ICE curves by x_var value
    ridgeline_data <- ice_data %>%
      select(all_of(x_var), fit, .id) %>%
      rename(x_value = !!sym(x_var))
    
    # Add categorical-specific elements to the plot
    p <- p +
      # Add ridgeline plots for each category
      geom_vridgeline(data = ridgeline_data,
                      aes(x = x_value,
                          y = fit,
                          width = after_stat(density)),
                      stat = "ydensity",
                      trim = FALSE,
                      scale = ridgeline_scale,
                      alpha = ridgeline_alpha,
                      color = "white") +
      
      # Add PDP mean points
      geom_point(
        data = pdp_data,
        aes(x = .data[[x_var]], y = fit),
        color = pdp_color,
        size = 3,
        shape = 21,
        fill = "white",
        stroke = 2
      ) +
      
      # Labels and theme
      labs(
        x = x_var, 
        y = "Latent Score / Category Mapping",
        title = title,
        subtitle = paste(subtitle, "(Ridgeline plot for categorical variable)")
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.5)
      )
    
  } else {
    # CONTINUOUS PLOT with ICE lines and PDP
    
    # Add continuous-specific elements to the plot
    p <- p +
      # Add ICE lines
      geom_line(
        data = ice_data,
        aes(x = !!sym(x_var), y = fit, group = .id),
        color = ice_color, 
        alpha = ice_alpha, 
        linewidth = ice_linewidth
      ) +
      
      # Add PDP mean line
      geom_line(
        data = pdp_data, 
        aes(x = !!sym(x_var), y = fit),
        color = pdp_color, 
        linewidth = pdp_linewidth
      ) +
      
      # Add PDP points
      geom_point(
        data = pdp_data,
        aes(x = !!sym(x_var), y = fit),
        color = pdp_color, 
        size = 2
      ) +
      
      # Set axis limits
      coord_cartesian(ylim = limits) +
      
      # Labels and theme
      labs(
        x = x_var,
        y = "Latent Score / Category Mapping",
        title = title,
        subtitle = subtitle
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "white", size = 0.5),
        panel.grid.major.y = element_line(color = "white", size = 0.5)
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
