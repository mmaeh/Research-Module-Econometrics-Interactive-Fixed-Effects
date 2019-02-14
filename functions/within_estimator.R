# This function implements the common OLS and within
# estimator. The latter is able to account for fixed
# effects in the individual and time dimension.

within_est <- function(X, Y, individual = FALSE, time = FALSE) {
  # Dimension of X.
  c(T, N, p) %<-% dim(X)
  
  # Initialize containers for XX, XY needed for OLS estimator.
  XX <- matrix(0, nrow = p, ncol = p)
  XY <- matrix(0, nrow = p, ncol = 1)
  # X_dem <- array(data = NA, dim = c(T, N, p))
  
  # Initialize transformation matrices, default is identity matrix,
  # which reduces to the normal OLS estimator. 
  M_N <- diag(1, N)
  M_T <- diag(1, T)
  
  if (individual == TRUE) {
    # Compute cross-section transformation matrix. 
    M_N <-
      diag(1, N) - ones(N) %*% solve(t(ones(N)) %*% ones(N)) %*% t(ones(N))
  }
  
  if (time == TRUE) {
    # Compute time-section transformation matrix. 
    M_T <-
      diag(1, T) - ones(T) %*% solve(t(ones(T)) %*% ones(T)) %*% t(ones(T))
  }
  
  for (j in 1:p) {
    for (k in j:p) {
      # Compute within-transformation for every regressor.
      X_j_transf <- M_T %*% X[, , j] %*% M_N
      # Also, Compute within-transformation for all 
      # regressors k != j.
      X_k_transf <- M_T %*% X[, , k] %*% M_N

      # Save trace of inner product of transformed regressors.
      XX[j, k] <- tr(t(X_j_transf) %*% X_k_transf)
      # The lower triangle of the matrix is symmetric to the
      # upper triangle.
      XX[k, j] <- XX[j, k]
      
      # Also, take the trace of the inner product between 
      # each regressor and Y.
      XY[j] <- tr(t(X_j_transf) %*% Y)
    }
  }
  
  # Compute OLS estimator from transformed data.
  beta <- solve(XX) %*% XY
  
  # Return estimate.
  return(beta)
  
}