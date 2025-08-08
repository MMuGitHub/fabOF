#' Partial Dependence Data Extraction for ranger Models
#'
#' Generates the data required to plot ICE curves and the aggregated partial
#' dependence (PDP) curve for a fitted \code{ranger} model, using the
#' \code{vivid::pdpVars} approach for grid creation, sampling, and prediction.
#' No plotting is performed — the output can be passed to custom plotting code.
#'
#' @param data A data frame containing the predictor variables and the response.
#' @param fit A fitted \code{ranger} model object.
#' @param response Character string. The name of the response variable in \code{data}.
#' @param vars Character vector of predictor variables for which PDP/ICE data should be generated.
#'   If \code{NULL}, all predictors except \code{response} are used.
#' @param pal Colour palette (unused here but kept for API compatibility).
#' @param gridSize Integer. Number of points to evaluate in the grid for each variable.
#' @param nmax Integer. Maximum number of rows sampled from \code{data} for ICE computation.
#' @param class Integer. Class index for classification models (default 1).
#' @param nIce Integer. Number of ICE curves to sample.
#' @param predictFun Optional custom prediction function. If \code{NULL}, uses the default from \code{vivid}.
#' @param limits Optional numeric vector of y-axis limits. If \code{NULL}, computed from the data.
#' @param colorVar Optional variable name for colouring ICE curves (instead of prediction values).
#' @param probability Logical. If \code{TRUE} for classification, predictions are probabilities.
#'
#' @return A list of length 2:
#'   \item{ice_data}{Tibble with ICE curve data (same structure as \code{pdp[[1]]$data}).}
#'   \item{pdp_data}{Tibble with aggregated PDP curve data (mean over ICE curves).}
#' @importFrom condvis2 CVpredict
#' @importFrom dplyr bind_rows filter group_by summarise
#' @importFrom tibble as_tibble
#' @importFrom labeling rpretty
#' @export
fun_pdp_data <- function(data,
                         fit,
                         response,
                         vars = NULL,
                         pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                         gridSize = 10,
                         nmax = 500,
                         class = 1,
                         nIce = 30,
                         predictFun = NULL,
                         limits = NULL,
                         colorVar = NULL,
                         probability = FALSE) {
  
  # Remove missing values
  data <- na.omit(data)
  
  # Handle nmax sampling
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)
  
  # Classification check
  classif <- is.factor(data[[response]]) || inherits(fit, "LearnerClassif")
  
  # Default predictFun from vivid
  if (is.null(predictFun)) {
    predictFun <- CVpredictfun(classif, class)
  }
  
  # Get base predictions for colour mapping
  if (classif) {
    predData <- predictFun(fit, data, prob = probability)
  } else {
    predData <- predictFun(fit, data)
  }
  
  # Determine predictor variables
  vars0 <- setdiff(names(data), response)
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0
  
  # ICE curve sampling
  if (length(nIce) > 1) {
    nIce <- nIce[nIce <= nrow(data)]
    sice <- c(NA, nIce)
  } else {
    nIce <- min(nIce, nrow(data))
    sice <- c(NA, sample(nrow(data), nIce))
  }
  data$predData <- predData
  
  # Create grid data for each variable
  pdplist1 <- vector("list", length = length(vars))
  for (i in seq_along(vars)) {
    px <- vivid:::pdp_data(data, vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- dplyr::bind_rows(pdplist1)
  
  # Predict on grid
  if (classif) {
    pdplist1$fit <- predictFun(fit, pdplist1, prob = probability)
  } else {
    pdplist1$fit <- predictFun(fit, pdplist1[, 1:(ncol(pdplist1) - 3)])
  }
  
  # Split by variable for easier handling
  pdplist1 <- split(pdplist1, pdplist1$.pid)
  names(pdplist1) <- vars
  
  # Determine y limits if needed
  if (is.null(limits)) {
    r <- sapply(pdplist1, function(x) range(x$fit))
    r <- range(c(r, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  }
  
  # For now only return first variable's data
  var <- vars[[1]]
  pdp <- pdplist1[[var]]
  
  # Aggregate mean PDP
  aggr <- pdp %>%
    dplyr::group_by(.data[[var]]) %>%
    dplyr::summarise(fit = mean(fit), .groups = "drop")
  
  # Filter ICE data for selected curves
  pdp1 <- dplyr::filter(pdp, .data[[".id"]] %in% sice)
  
  # Return as list
  list(
    ice_data = tibble::as_tibble(pdp1),
    pdp_data = tibble::as_tibble(aggr)
  )
}
