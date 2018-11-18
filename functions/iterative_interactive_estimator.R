interactive_est <-
  function(X, Y, r, tolerate) {
    c(t, i, p) %<-% dim(X)
    betanorm <- 1
    betaold <- matrix(0, nrow = p, ncol = 1)
    
    xxinv <- xx_inv(X)
    
    beta_start <- matrix(0, nrow = 2, ncol = p)
    beta_opt <- matrix(NA, nrow = 2, ncol = p)
    sigma_opt <- matrix(NA, nrow = 2, ncol = 1)
    beta_start[1,] <- within_est(X, Y, individual = FALSE, time = FALSE)[1:p]
    beta_start[2,] <- within_est(X, Y, individual = TRUE, time = TRUE)[1:p]
    
    for (j in 1:1) {
    
      beta <- beta_start[j,]  
            
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
      
      beta_opt[j,] <- beta
      
      sigma_opt[j] <- sigma 
    
    }
    
    return(c(beta_opt[which.min(sigma_opt),], sd))
    
  }