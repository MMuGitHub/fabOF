#' Create Partial Dependence Plot with Individual Conditional Expectation Lines
#'
#' This function creates partial dependence plots (PDP) with individual conditional 
#' expectation (ICE) lines. It automatically detects whether the predictor variable 
#' is continuous or categorical and creates appropriate visualizations. For continuous 
#' variables, it shows ICE lines with optional category background mapping. For 
#' categorical variables, it creates ridgeline plots showing the distribution of 
#' predictions for each category.
#'
#' @param pdp_list A list containing ICE and PDP data, typically the output from 
#'   \code{fun_pdp_data()}. Must contain elements \code{ice_data} and \code{pdp_data}.
#' @param x_var Character string specifying the name of the predictor variable to plot.
#' @param borders Numeric vector of category borders for continuous variables. 
#'   If NULL, no background category mapping is shown. Default is NULL.
#' @param category_colors Character vector of colors for category backgrounds. 
#'   Default is c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C').
#' @param category_names Character vector of category names for the legend. 
#'   Default is c("very american", "american", "neutral", "british", "very british").
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
#' @return A ggplot object containing the partial dependence plot.
#'
#' @details
#' The function automatically detects the variable type:
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
#' # Assuming you have trained a model and prepared data
#' pdp_result <- fun_pdp_data(
#'   data = dm_train_dummy, 
#'   fit = rf_ord_dummy$ranger.fit,
#'   response = "score",
#'   vars = c("item01"),
#'   gridSize = 250,
#'   nIce = 20
#' )
#' 
#' # Create plot for continuous variable with category mapping
#' plot_continuous <- fun_pdp_plot(
#'   pdp_result, 
#'   x_var = "item01",
#'   borders = rf_ord$category.borders,
#'   category_colors = c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C'),
#'   category_names = c("very american", "american", "neutral", "british", "very british")
#' )
#' 
#' # Create plot for categorical variable (ridgeline plot)
#' plot_categorical <- fun_pdp_plot(
#'   pdp_result_cat, 
#'   x_var = "factor_variable"
#' )
#' 
#' print(plot_continuous)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import ggridges
#' @importFrom labeling rpretty
#' @importFrom rlang sym
#'
#' @seealso \code{\link{fun_pdp_data}} for generating the input data
#'
#' @author Your Name
#' @export
fun_pdp_plot <- function(pdp_list, 
                         x_var,
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
  
  #create color palette:
  
  if(is.null(category_colors)){
    
    colors <- c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C')
    # Create a palette-generating function
    color_palette <- colorRampPalette(colors)
    category_colors <- color_palette(length(borders)-1)
    
  }
  
  # Extract data from list
  ice_data <- pdp_list$ice_data
  pdp_data <- pdp_list$pdp_data
  
  # Check if x_var should be treated as categorical
  # Categorical if: factor, character, or numeric with 4 or fewer unique values
  unique_values <- length(unique(ice_data[[x_var]]))
  is_factor_or_char <- is.factor(ice_data[[x_var]]) || is.character(ice_data[[x_var]])
  is_few_values <- unique_values <= 4
  
  is_categorical <- is_factor_or_char || is_few_values
  
  # Warning if treating numeric variable as categorical due to few values
  if (!is_factor_or_char && is_few_values) {
    warning(paste0("Variable '", x_var, "' has only ", unique_values, 
                   " unique values. Treating as categorical and using ridgeline plot."))
  }
  
  # Get limits for y-axis
  limits <- range(labeling::rpretty(
    min(c(ice_data$fit, pdp_data$fit)),
    max(c(ice_data$fit, pdp_data$fit))
  ))
  
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
    
    if(is.null(category_names)){
      category_names <- paste0("cat", seq_len(length(borders)-1))
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
                      aes(x=x_value,
                          y=fit,
                          width = after_stat(density)),
                      stat="ydensity",
                      trim=FALSE,
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
  
  return(p)
}

# Example usage:
# Assuming you have your pdp data from fun_pdp_data:
# pdp_result <- fun_pdp_data(data = dm_train_dummy, 
#                            fit = rf_ord_dummy$ranger.fit,
#                            response = "score",
#                            vars = c("item01"),
#                            gridSize = 250,
#                            nIce = 20)
# 
# # Create the plot
# plot <- fun_pdp_plot(pdp_result, 
#                      x_var = "item01",
#                      borders = rf_ord$category.borders,
#                      category_colors = c('#4DAF4A', '#377EB8', '#FFFF33', '#FF7F00', '#E41A1C'),
#                      category_names = c("very american", "american", "neutral", "british", "very british"))
# 
# print(plot)