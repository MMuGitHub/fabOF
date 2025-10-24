# Internal function used in mixfabOF for computing the log likelihood.
logLik <- function(Z, id, b.hat, eps.hat, D.hat, sigma.sq.hat) {
  ll <- 0
  n.grp <- length(unique(id))

  for (j in 1:n.grp) {
    id.j <- which(id == unique(id)[j])
    R.j <- diag(sigma.sq.hat, length(id.j))
    ll <- ll +
      t(eps.hat[id.j]) %*% solve(R.j) %*% eps.hat[id.j] +
      t(b.hat[j, ]) %*% solve(D.hat) %*% b.hat[j, ] +
      determinant(D.hat, logarithm = TRUE)$modulus[1] +
      determinant(R.j, logarithm = TRUE)$modulus[1]
  }
  return(-as.numeric(ll))
}
