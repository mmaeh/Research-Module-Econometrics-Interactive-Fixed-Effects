infeasible_est <- function(X, Y, given = NULL) {
  if (given == 'factors') {
    M <- diag(1, t) - fac %*% solve(t(fac) %*% fac) %*% t(fac)
  } else {
    if (given == 'loadings') {
      M <- diag(1, i) - load %*% solve(t(load) %*% load) %*% t(load)
    } else {
      stop('given has to be either factors or loadings')
    }
  }
  
  c(t, i, p) %<-% dim(X)
  xx <- matrix(0, nrow = p, ncol = p)
  xy <- matrix(0, nrow = p, ncol = 1)
  
  if (p == 1) {
    xx[1, 1] <- tr(t(X) %*% M %*% X)
    xy[1] <- tr(t(X) %*% M %*% Y)
  }
  
  else {
    for (j in 1:p) {
      for (k in j:p) {
        MX1 <- M %*% X[, , j]
        MX2 <- M %*% X[, , k]
        xx[j, k] <- tr(t(MX1) %*% MX2)
        xx[k, j] <- xx[j, k]
        
        xy[j] <- tr(t(MX1) %*% Y)
      }
    }
  }
  
  beta <- solve(xx) %*% xy

  'W_hat <- Y
  for (j in 1:length(param)) {
    W_hat <- W_hat - X[,,j] * beta[j]
  }
  load_hat <- t(W_hat) %*% fac / t
    
  err <- Y - fac %*% t(load_hat)
  for (j in 1:length(param)) {
    err <- err - X[,,j] * beta[j]
  }
  sigma <- tr(err %*% t(err)) / (i * t + r^2 - p)
  
  D_F <- parameter_variance(X, fac, load_hat)
  D_F_diag <- diag(D_F)
  
  variance <- sigma * D_F
  sd <- sqrt(diag(variance))'
  
  return(beta)
  
}
