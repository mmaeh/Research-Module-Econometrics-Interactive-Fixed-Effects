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
  
  err <- Y - X[,,1] * beta[1] - X[,,2] * beta[2] - fac %*% t(load)
  sigma <- tr(err %*% t(err)) / (i * t + r^2 - p)
    
  W_hat <- Y - X[,,1] * beta[1] - X[,,2] * beta[2]
  load_hat <- t(W_hat) %*% fac / t
  
  D_F <- parameter_variance(X, fac, load_hat)
  D_F_diag <- diag(D_F)
  
  variance <- sigma * D_F
  sd <- sqrt(diag(variance))
  
  return(c(beta, sd, sigma, D_F_diag))
  
}
