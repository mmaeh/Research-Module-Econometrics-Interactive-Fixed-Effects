factor_est <- function(W, R) {
  c(T, N) %<-% dim(W)
  
  if (T < N) {
    WW <- W %*% t(W) / (T * N)
    ev <- eigen(WW)
    fac_hat <- ev$vectors[, 1:R] * sqrt(T)
    load_hat <- t(W) %*% fac_hat / T
  }  else {
    WW <- t(W) %*% W / (T * N)
    ev <- eigen(WW)
    load_hat <- ev$vectors[, 1:R] * sqrt(N)
    fac_hat <- W %*% load_hat / N
  }
  
  eigenvalues <- diag(ev$values[1:R])
  
  return(list(fac_hat, load_hat, eigenvalues))
  
}
