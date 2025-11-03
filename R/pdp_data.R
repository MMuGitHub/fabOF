#' Create PDP Data Grid
#'
#' Internal helper function adapted from the vivid package to create a grid
#' of values for partial dependence plot calculations.
#'
#' @param d A data frame
#' @param var Character vector of variable name(s) (1 or 2 variables)
#' @param gridsize Integer specifying the number of grid points
#' @param convexHull Logical indicating whether to restrict to convex hull
#'
#' @return A data frame with the grid of values
#'
#' @keywords internal
pdp_data <- function(d, var, gridsize = 30, convexHull = FALSE) {
  if (length(var) == 1) {
    pdpvar <- d[[var]]
    if (is.factor(pdpvar)) {
      gridvals <- levels(pdpvar)
    } else {
      gridvals <- seq(min(pdpvar, na.rm = T), max(pdpvar, na.rm = T), length.out = gridsize)
    }
    dnew <- do.call(rbind, lapply(gridvals, function(i) {
      d1 <- d
      d1[[var]] <- i
      d1
    }))
    if (is.factor(pdpvar)) dnew[[var]] <- factor(dnew[[var]], levels = levels(pdpvar), ordered = is.ordered(pdpvar))
  } else {
    pdpvar1 <- d[[var[1]]]
    pdpvar2 <- d[[var[2]]]

    if (is.factor(pdpvar1)) {
      gridvals1 <- levels(pdpvar1)
    } else {
      gridvals1 <- seq(min(pdpvar1, na.rm = T), max(pdpvar1, na.rm = T), length.out = gridsize)
    }
    if (is.factor(pdpvar2)) {
      gridvals2 <- levels(pdpvar2)
    } else {
      gridvals2 <- seq(min(pdpvar2, na.rm = T), max(pdpvar2, na.rm = T), length.out = gridsize)
    }
    gridvals <- expand.grid(gridvals1, gridvals2)

    if (convexHull) {
      if (is.factor(pdpvar1) && is.factor(pdpvar2)) {
        t <- table(pdpvar1, pdpvar2)
        w <- sapply(1:nrow(gridvals), function(i) t[gridvals[i, 1], gridvals[i, 2]] == 0)
        gridvals <- gridvals[!w, ]
      } else if (is.factor(pdpvar1) && is.numeric(pdpvar2)) {
        rangeData <- tapply(pdpvar2, pdpvar1, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 1])]]
          gridvals[i, 2] >= r[1] && gridvals[i, 2] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else if (is.numeric(pdpvar1) && is.factor(pdpvar2)) {
        rangeData <- tapply(pdpvar1, pdpvar2, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 2])]]
          gridvals[i, 1] >= r[1] && gridvals[i, 1] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else {
        hpts <- chull(pdpvar1, pdpvar2) # calc CHull
        hpts <- c(hpts, hpts[1]) # close polygon
        pdpvar1CH <- pdpvar1[hpts] # get x-coords of polygon
        pdpvar2CH <- pdpvar2[hpts] # get y-coords of polygon

        # find which are outside convex hull
        res <- sp::point.in.polygon(gridvals$Var1, gridvals$Var2, pdpvar1CH, pdpvar2CH) != 0

        # remove points outside convex hull
        gridvals <- gridvals[res, ]
      }
    }

    dnew <- do.call(rbind, lapply(1:nrow(gridvals), function(i) {
      d1 <- d
      d1[[var[1]]] <- gridvals[i, 1]
      d1[[var[2]]] <- gridvals[i, 2]
      d1
    }))
    if (is.factor(pdpvar1)) dnew[[var[1]]] <- factor(dnew[[var[1]]], levels = levels(pdpvar1), ordered = is.ordered(pdpvar1))
    if (is.factor(pdpvar2)) dnew[[var[2]]] <- factor(dnew[[var[2]]], levels = levels(pdpvar2), ordered = is.ordered(pdpvar2))
  }
  # making sure the repeats is consistent
  n_repeats <- nrow(dnew) / nrow(d)
  dnew$.id <- rep(1:nrow(d), times = ceiling(n_repeats))

  # dnew$.id <- 1:nrow(d)
  rownames(dnew) <- NULL
  dnew
}