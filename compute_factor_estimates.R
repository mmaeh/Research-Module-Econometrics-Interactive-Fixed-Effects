factor_est <- function(W, r) {
  c(t, i) %<-% dim(W)
  
  if (t < i) {
    WW <- W %*% t(W) / (t * i)
    #WW <- W %*% t(W) / (t * i)
    #c(eigval, fhat0, fhat1) %<-% svd(WW, nu = nrow(WW), nv = ncol(WW))
    #fac_hat <- fhat0[, 1:r] * sqrt(t)
    ev <- eigen(WW)
    fac_hat <- ev$vectors[, 1:r] * sqrt(t)
    load_hat <- t(W) %*% fac_hat / t
  }  else {
    WW <- t(W) %*% W / (t * i)
    #WW <- t(W) %*% W / (t * i)
    #c(eigval, fhat0, fhat1) %<-% svd(WW, nu = nrow(WW), nv = ncol(WW))
    #load_hat <- fhat0[, 1:r] * sqrt(i)
    ev <- eigen(WW)
    load_hat <- ev$vectors[, 1:r] * sqrt(i)
    fac_hat <- W %*% load_hat / i
  }
  
  VNT <- diag(ev$values[1:r])
  
  return(list(fac_hat, load_hat, VNT))
  
}
