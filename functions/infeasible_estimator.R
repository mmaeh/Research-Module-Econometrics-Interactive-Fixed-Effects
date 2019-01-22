infeasible_est <- function(X, Y, given = NULL) {
  
  # Specify transformation matrix M, depending on whether factors
  # or loadings you assume as given.
  if (given == 'factors') {
    M <- diag(1, t) - fac %*% solve(t(fac) %*% fac) %*% t(fac)
  } else if (given == 'loadings') {
    M <- diag(1, i) - load %*% solve(t(load) %*% load) %*% t(load)
  } else {
    stop('Given has to be either factors or loadings')
  }
  
  # Dimensions of X.
  c(T, N, p) %<-% dim(X)
  
  # Containers to store XY and XX estimates.
  XX <- matrix(0, nrow = p, ncol = p)
  XY <- matrix(0, nrow = p, ncol = 1)
  
  # If p == 1, XX and XY are simply weighted by M.
  if (p == 1) {
    XX[1, 1] <- tr(t(X) %*% M %*% X)
    XY[1] <- tr(t(X) %*% M %*% Y)
  }
  
  # For p > 1 compute XX and XY entries from traces of 
  # inner products of MX_j and MX_k, i.e. the transformed X matrices.
  else {
    for (j in 1:p) {
      for (k in j:p) {
        MX_j <- M %*% X[, , j] # Transformed X along regressor j.
        MX_k <- M %*% X[, , k] # Transformed X along regressor k.
        XX[j, k] <- tr(t(MX_j) %*% MX_k) # XX entry at j, k.
        XX[k, j] <- xx[j, k] # This is just symmetric to the upper triangle of XX.
        
        XY[j] <- tr(t(MX_j) %*% Y) # XY entry at j.
      }
    }
  }
  
  # Compute OLS estimator.
  beta <- solve(XX) %*% XY
  
  return(beta)
  
}
