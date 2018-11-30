interactive_est_3 <-
  function(X, Y, r, tolerate) {
    # dimensions of X
    c(t, i, p) %<-% dim(X)
    
    # optimize containers
    beta_start <- matrix(0, nrow = 2, ncol = p)
    beta_start[2, ] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    # things to intialize before while loop
    xxinv <- xx_inv(X)
    beta_opt <- matrix(NA, nrow = 2, ncol = p)
    sigma_opt <- matrix(NA, nrow = 2, ncol = 1)

    for (s in 1:2) {

      beta <- beta_start[s,]
      betaold <- beta
      betanorm <- 1
      n <- 0
  
      
      # while loop to get interactive estimator 
      while (n < 500 && betanorm > tolerate) {
        
        # get factor and loading estimates
        W <- Y
        for (h in 1:p) {
          W <- W - X[, , h] * beta[h]
        }
        c(fac_hat, load_hat, VNT) %<-% factor_est(W, r)
        
        # update beta and compare to previous estimate
        beta <- beta_update(X, xxinv, Y, fac_hat, load_hat)
        betanorm <- norm(beta - betaold)
        betaold <- beta
        
        # set counter + 1
        n <- n + 1
        
      }
      
      err <- Y - fac_hat %*% t(load_hat)
      for (n in 1:length(beta)) {
        err <- err - X[,,n] * beta[n]
      }
      
      sigma_opt[s] <- tr(err %*% t(err)) 
      
      beta_opt[s,] <- beta
      
    }
      
    return(beta_opt[which.min(sigma_opt),])
    
  }
