# Internal function used for computing prediction error for variable importance.
linearKappa <- function(y, ypred) {
  n.cats <- length(levels(y))
  # Linear weight matrix: w_ij = 1 - |i-j|/(k-1)
  weight.mat <- 1 - abs(outer(1:n.cats, 1:n.cats, "-")) / (n.cats - 1)

  # Confusion matrix (normalized to proportions)
  conf <- table(
    factor(y, levels = levels(y)),
    factor(ypred, levels = levels(y))
  )
  conf <- conf / sum(conf)

  # Marginals
  row.marginals <- rowSums(conf)
  col.marginals <- colSums(conf)

  # Expected agreement under independence
  expected <- outer(row.marginals, col.marginals)

  # Weighted observed and expected agreement
  observed.agreement <- sum(weight.mat * conf)
  expected.agreement <- sum(weight.mat * expected)

  # Weighted kappa
  (observed.agreement - expected.agreement) / (1 - expected.agreement)
}
