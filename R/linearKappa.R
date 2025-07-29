# Internal function used for computing prediction error for variable importance.
#' @import psych
linearKappa <- function(y, ypred) {
  n.cats <- length(levels(y))
  ydat <- data.frame(y = y, ypred = ypred)
  weight.mat <- abs(t(replicate(n.cats, 1:n.cats)) - replicate(n.cats, 1:n.cats))
  weight.mat <- 1 - weight.mat/(n.cats - 1)
  psych::cohen.kappa(ydat, w = weight.mat, levels = levels(y))$weighted.kappa
}
