interactive_est_2 <-
  function(X, Y, r, tolerate) {
    c(t, i, p) %<-% dim(X)
    
    betanorm <- 1
    betaold <- matrix(0, nrow = p, ncol = 1)
    
    xxinv <- xx_inv(X)
    
    W <- Y
    c(fac_hat, load_hat, VNT) %<-% factor_est(W, r)
    
    n <- 0
    while (n < 500 && betanorm > tolerate) {
      n <- n + 1
      beta <- beta_update(X, xxinv, Y, fac_hat, load_hat)
      betanorm <- norm(beta - betaold)
      betaold <- beta
      W <- Y
      
      for (h in 1:p) {
        W <- W - X[, , h] * beta[h]
      }
      
      c(fac_hat, load_hat, VNT) %<-% factor_est(W, r)
    
    }
      
    err <- Y - fac_hat %*% t(load_hat)
    for (n in 1:length(param)) {
      err <- err - X[,,n] * beta[n]
    }
    
    sigma <- tr(err %*% t(err)) / ((i - r) * (t - r) - p)
    
    D_F <- parameter_variance(X, fac_hat, load_hat)
    D_F_diag <- diag(D_F)
    
    variance <- sigma * D_F
    sd <- sqrt(diag(variance))
    
    return(c(beta, sd))
    
  }