#' Prints description of fabOF model object.
#'
#' @title Print fabOF
#' @param x Object of class fabOF
#' @param ... Further arguments passed to or from other methods.
#' @author Philip Buczak
#' @export
print.fabOF <- function(x, ...) {
  # Inspired by ranger and ordinalForest packages.
  cat("Frequency Adjusted Borders Ordinal Forest (fabOF)\n\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Number of trees: ", x$ranger.fit$num.trees, "\n")
  cat("Observations:    ", x$ranger.fit$num.samples, "\n")
  cat("Covariates:      ", x$ranger.fit$num.independent.variables, "\n")
  cat("Target variable: ", x$target, "\n")
  cat(paste0("Categories:       ", x$categories[1],
             " (n = ", x$category.frequencies[1], ")", "\n"))
  for(i in 2:length(x$categories)) {
    cat(paste0("                  ", x$categories[i],
               " (n = ", x$category.frequencies[i], ")", "\n"))
  }
  cat("Category scores: ",  x$category.scores, "\n")
  cat("Category borders:", x$category.borders, "\n")
}
