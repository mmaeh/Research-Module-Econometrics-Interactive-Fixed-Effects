factor_est <- function(W, r) {
  c(t, i) %<-% dim(W)
  
  if (t < i) {
    WW <- W %*% t(W) / (t * i)
    ev <- eigen(WW)
    fac_hat <- ev$vectors[, 1:r] * sqrt(t)
    load_hat <- t(W) %*% fac_hat / t
  }  else {
    WW <- t(W) %*% W / (t * i)
    ev <- eigen(WW)
    load_hat <- ev$vectors[, 1:r] * sqrt(i)
    fac_hat <- W %*% load_hat / i
  }
  
  eigenvalues <- diag(ev$values[1:r])
  
  return(list(fac_hat, load_hat, eigenvalues))
  
}
