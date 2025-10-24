#' Plot Variable Importance for fabOF or mixfabOF Models
#'
#' This function creates a horizontal bar plot showing variable importance scores
#' from fitted fabOF or mixfabOF models. Variables are automatically sorted by
#' importance and displayed with customizable styling options.
#'
#' @param model A fitted \code{fabOF} or \code{mixfabOF} model object containing
#'   variable importance information.
#' @param top_n Integer. Number of top variables to display. If \code{NULL} (default),
#'   all variables are shown. If specified, only the top N most important variables
#'   are displayed.
#' @param title Character string for the plot title. Default is "Variable Importance".
#' @param subtitle Character string for the plot subtitle. Default is \code{NULL}.
#' @param x_title Character string for the x-axis title. Default is "Importance Score".
#' @param y_title Character string for the y-axis title. Default is "Variables".
#' @param bar_color Character string specifying the color of the bars. Default is "#008080".
#' @param bar_alpha Numeric value between 0 and 1 for bar transparency. Default is 0.8.
#' @param text_color Character string specifying the color of the importance value labels
#'   on the bars. Default is "white".
#' @param text_size Numeric value for the size of the importance value labels. Default is 3.
#' @param show_values Logical. If \code{TRUE} (default), importance values are displayed
#'   on the bars. If \code{FALSE}, no values are shown.
#' @param sort_ascending Logical. If \code{FALSE} (default), variables are sorted in
#'   descending order of importance (most important at top). If \code{TRUE}, sorted
#'   in ascending order.
#' @param threshold Numeric. Optional threshold value to highlight variables above/below
#'   this importance score. If \code{NULL} (default), no threshold line is shown.
#' @param threshold_color Character string specifying the color of the threshold line.
#'   Default is "red".
#' @param digits Integer. Number of decimal places to display in value labels.
#'   Default is 4.
#'
#' @return A ggplot object showing the variable importance plot.
#'
#' @details
#' The function extracts variable importance scores from the ranger model within
#' fabOF or mixfabOF objects and creates a horizontal bar plot. Variables are
#' automatically sorted by importance score, and various customization options
#' are available for styling.
#'
#' Negative importance values are supported and will be displayed with bars
#' extending in the negative direction.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot_importance(model = rf_ord_dummy)
#'
#' # Show only top 10 variables
#' plot_importance(model = rf_ord_dummy, top_n = 10)
#'
#' # Customize appearance
#' plot_importance(
#'   model = rf_ord_dummy,
#'   top_n = 15,
#'   title = "Top 15 Most Important Variables",
#'   bar_color = "steelblue",
#'   threshold = 0.01,
#'   show_values = FALSE
#' )
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tibble tibble
#'
#' @export
plot_importance <- function(
  model,
  top_n = NULL,
  title = "Variable Importance",
  subtitle = NULL,
  x_title = "Importance Score",
  y_title = "Variables",
  bar_color = "#008080",
  bar_alpha = 0.8,
  text_color = "white",
  text_size = 3,
  show_values = TRUE,
  sort_ascending = FALSE,
  threshold = NULL,
  threshold_color = "red",
  digits = 4
) {
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  require(tibble)

  # Input validation
  if (!inherits(model, c("mixfabOF", "fabOF"))) {
    stop("Model must be a 'mixfabOF' or 'fabOF' object.")
  }

  # Extract variable importance from model object
  if (is.null(model$variable.importance)) {
    stop(
      "Variable importance not available. Make sure the model was fitted with importance = 'permutation' or 'impurity'."
    )
  }

  importance_scores <- model$variable.importance

  # Convert to tibble for easier manipulation
  importance_data <- tibble(
    variable = names(importance_scores),
    importance = as.numeric(importance_scores)
  )

  # Sort by importance
  if (sort_ascending) {
    importance_data <- importance_data %>%
      arrange(importance)
  } else {
    importance_data <- importance_data %>%
      arrange(desc(importance))
  }

  # Filter to top_n if specified
  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) {
    importance_data <- importance_data %>%
      slice_head(n = top_n)
  }

  # Reorder factor levels for proper plotting order
  if (sort_ascending) {
    importance_data$variable <- factor(
      importance_data$variable,
      levels = importance_data$variable
    )
  } else {
    importance_data$variable <- factor(
      importance_data$variable,
      levels = rev(importance_data$variable)
    )
  }

  # Create base plot
  p <- ggplot(importance_data, aes(x = importance, y = variable)) +
    geom_col(fill = bar_color, alpha = bar_alpha, width = 0.7) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      axis.text.y = element_text(hjust = 1)
    )

  # Add value labels if requested
  if (show_values) {
    # Position labels at the right end of bars
    importance_data <- importance_data %>%
      mutate(
        label_pos = ifelse(
          importance >= 0,
          importance + max(abs(importance)) * 0.02,
          importance - max(abs(importance)) * 0.02
        ),
        label_hjust = ifelse(importance >= 0, 0, 1)
      )

    p <- p +
      geom_text(
        data = importance_data,
        aes(x = label_pos, y = variable, label = round(importance, digits)),
        color = "black", # Changed to black for better visibility outside bars
        size = text_size,
        hjust = importance_data$label_hjust
      )
  }

  # Add threshold line if specified
  if (!is.null(threshold) && is.numeric(threshold)) {
    p <- p +
      geom_vline(
        xintercept = threshold,
        color = threshold_color,
        linetype = "dashed",
        alpha = 0.8,
        size = 0.8
      )
  }

  return(p)
}
