beta_update <- function(X, xxinv, Y, fac_hat, load_hat) {
  c(t, i, p) %<-% dim(X)
  
  xy <- matrix(0, nrow = p, ncol = 1)
  
  for (k in 1:p) {
    xy[k] <- tr(t(X[, , k]) %*% (Y - fac_hat %*% t(load_hat)))
  }
  
  beta <- xxinv %*% xy
  
  return(beta)
}
