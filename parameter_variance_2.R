parameter_variance <- function(X, fac, load) {
  c(t, i, p) %<-% dim(X)
  
  #compute projection matrix
  
  M_F <- diag(t) - fac %*% solve(t(fac) %*% fac) %*% t(fac)
  M_L <- diag(i) - load %*% solve(t(load) %*% load) %*% t(load)
  
  #compute variance matrix
  
  D_F <- matrix(0, nrow = p, ncol = p)
  
  for (j in 1:p) {
    for (k in 1:p) {
      D_F[j, k] <- 1 / (t * i) * tr(M_L %*% t(X[,,j]) %*% M_F %*% X[,,k])
    }
  }
  
  D_F <- 1 / (t * i) * solve(D_F) #first term cause of root TN consistency, p. 1241
  
  return(D_F)
  
}
