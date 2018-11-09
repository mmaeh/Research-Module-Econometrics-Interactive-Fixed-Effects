parameter_variance <- function(X, fac, load) {
  c(t, i, p) %<-% dim(X)
  
  #compute matrix A
  
  A <- matrix(0, nrow = i, ncol = i)
  
  for (j in 1:i) {
    for (k in 1:i) {
      A[j, k] <- t(load[j, ]) %*% solve((t(load) %*% load) / i) %*% load[k, ]
    }
  }
  
  #compute projection matrix
  
  M_F <- diag(t) - fac %*% solve(t(fac) %*% fac) %*% t(fac)
  
  #compute variance matrix
  
  D_F <- matrix(0, nrow = p, ncol = p)
  
  for (j in 1:i) {
    Z_i <- M_F %*% X[, j, ]
    for (k in 1:i) {
      Z_i <- Z_i - 1 / i * M_F %*% X[, k, ] * A[j, k]
    }
    D_F <- D_F + t(Z_i) %*% Z_i
  }
  
  D_F <- 1 / (t * i) * solve(1 / (t * i) * D_F) #first term cause of root TN consistency, p. 1241
  
  return(D_F)
  
}
