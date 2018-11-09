xx_inv <- function(X) {
  c(t, i, p) %<-% dim(X)
  xx <- matrix(0, nrow = p, ncol = p)
  
  if (p == 1) {
    xx[1, 1] <- tr(t(X) %*% X)
  }
  
  else {
    for (k in 1:p) {
      X1 <- X[, , k]
      for (m in k:p) {
        X2 <- X[, , m]
        xx[k, m] <- tr(t(X1) %*% X2)
          if (k < m) {
          xx[m, k] <- xx[k, m]
          }
      }
    }
  }
  
  xxinv = solve(xx)
  
  return(xxinv)
  
}