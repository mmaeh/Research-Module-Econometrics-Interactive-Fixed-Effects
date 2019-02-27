# This function computes the beta update within the
# IFE estimator (see function interactive_estimator.R)
# as mentioned in Ba (2009) p. 1237..

beta_update <- function(X, XXinv, Y, fac_hat, load_hat) {
  # Dimensions of X.
  c(T, N, p) %<-% dim(X)
  
  # Initialize container to store XY.
  XY <- matrix(0, nrow = p, ncol = 1)
  
  # Compute p entries of XJ matrix according to the second
  # term of the IFE estimator on page 1237 of Bai (2009).
  for (j in 1:p) {
    XY[j] <- tr(t(X[, , j]) %*% (Y - fac_hat %*% t(load_hat)))
  }
  
  # Compute beta from usual OLS formula.
  beta <- XXinv %*% XY
  
  return(beta)
}
