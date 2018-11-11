interactive_est <-
  function(X, Y, r, tolerate) {
    c(t, i, p) %<-% dim(X)
    betanorm <- 1
    betaold <- matrix(0, nrow = p, ncol = 1)
    
    xxinv <- xx_inv(X)
    beta <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    W <- Y
    for (k in 1:length(param)) {
      W <- W - X[,,k] * beta[k]
    }
    
    c(fac_hat, load_hat, VNT) %<-% factor_est(W, r)
    
    n <- 0
    while (n < 500 && betanorm > tolerate) {
      n <- n + 1
      beta <- beta_update(X, xxinv, Y, fac_hat, load_hat)
      betanorm <- norm(beta - betaold)
      betaold <- beta
      W <- Y
      
      for (k in 1:p) {
        W <- W - X[, , k] * beta[k]
      }
      
      c(fac_hat, load_hat, VNT) %<-% factor_est(W, r)
    }

    err <- Y - X[,,1] * beta[1] - X[,,2] * beta[2] - fac_hat %*% t(load_hat)
    sigma <- tr(err %*% t(err)) / (i * t - r*(i + t) + r^2 - p)
        
    D_F <- parameter_variance(X, fac_hat, load_hat)
    D_F_diag <- diag(D_F)
    
    variance <- sigma * D_F
    sd <- sqrt(diag(variance))
    
    return(c(beta, sd, sigma, D_F_diag))
    
  }