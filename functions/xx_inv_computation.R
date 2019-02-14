# This function computes the inverse of a
# multidimensional array X by computing
# the traces of the inner products of all
# regressors. Within the IFE estimator 
# scheme the inverse has to be computed 
# only once.

xx_inv <- function(X) {
  
  # Get dimensions of X.
  c(T, N, p) %<-% dim(X)
  
  # Container to store XX matrix.
  XX <- matrix(0, nrow = p, ncol = p)
  
  # Compute XX matrix from traces of inner 
  # products of regressors. 
  for (j in 1:p) {
    X_j <- X[, , j]
    for (k in j:p) {
      X_k <- X[, , k]
      XX[j, k] <- tr(t(X_j) %*% X_k)
      XX[k, j] <- XX[j, k]
    }
  }
  
  # Compute inverse of XX.
  XXinv = solve(XX)
  
  return(XXinv)
  
}