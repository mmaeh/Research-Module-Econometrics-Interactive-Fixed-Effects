# This function implements the IFE estimator as 
# proposed by Bai (2009). The estimator is computed
# for two starting values, the OLS estimator and the 
# inital estimate of F.

interactive_est_3 <- function(X, Y, R, tolerate) {
    # Dimensions of X.
    c(T, N, p) %<-% dim(X)
    
    # Initialize containers to store estimates resulting
    # from different starting values, OLS estimate and 
    # PCA estimate of F.
    beta_start <- matrix(0, nrow = 2, ncol = p)
    beta_start[2, ] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    # Once compute matrix inverse of XX and initialize containers
    # to store beta and sigma estimates from each starting value.
    XXinv <- xx_inv(X)
    beta_opt <- matrix(NA, nrow = 2, ncol = p)
    sigma_opt <- matrix(NA, nrow = 2, ncol = 1)

    # Loop through starting values.
    for (s in 1:2) {

      beta <- beta_start[s,]
      betaold <- beta 
      betanorm <- 1 # To avoid convergence in first iteration.
      n <- 0
  
      
      # Run loop still convergence criterium is reached. 
      while (n < 500 && betanorm > tolerate) {
        
        # Initialize W = Y and adjust in the following
        # according to starting value.
        W <- Y
        
        # For F as starting value we simply get W = Y, 
        # since beta_start[1,] = 0.
        for (h in 1:p) {
          W <- W - X[, , h] * beta[h]
        }
        
        # Get factor and loading estimates from 
        # factor_est() function.
        c(fac_hat, load_hat, VNT) %<-% factor_est(W, R)
        
        # Update beta and compare to previous estimate to
        # check wheter convergence is reached.
        beta <- beta_update(X, XXinv, Y, fac_hat, load_hat)
        betanorm <- norm(beta - betaold)
        
        # Set beta to betaold, for next comparison in next
        # iteration step.
        betaold <- beta
        
        # Set counter plus one.
        n <- n + 1
        
      }
      
      # Compute errors for each starting value.
      err <- Y - fac_hat %*% t(load_hat)
      for (n in 1:length(beta)) {
        err <- err - X[,,n] * beta[n]
      }
      
      # Compute sigma corresponding to starting
      # value.
      sigma_opt[s] <- tr(err %*% t(err)) 
      
      # Save beta estimate.
      beta_opt[s,] <- beta
      
    }
    
    # Return beta corresponding to lower
    # cirterium value.
    return(beta_opt[which.min(sigma_opt),])
    
  }
