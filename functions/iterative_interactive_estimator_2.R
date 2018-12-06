interactive_est_2 <-
  function(X, Y, R, beta_start = "factor", tolerate) {
    # dimensions of X
    c(t, i, p) %<-% dim(X)
    
    # initialize beta values
    if (beta_start == "OLS") {
      beta <- within_est(X, Y, individual = FALSE, time = FALSE)
    } else if (beta_start == "within") {
      beta <- within_est(X, Y, individual = TRUE, time = TRUE)
    } else {
      beta <- matrix(0, nrow = p)
    }
    
    betaold <- beta

    # things to intialize before while loop
    xxinv <- xx_inv(X)        
    betanorm <- 1
    n <- 0
    
    # while loop to get interactive estimator 
    while (n < 500 && betanorm > tolerate) {
      
      # get factor and loading estimates
      W <- Y
      for (h in 1:p) {
        W <- W - X[, , h] * beta[h]
      }
      c(fac_hat, load_hat, VNT) %<-% factor_est(W, R)
      
      # update beta and compare to previous estimate
      beta <- beta_update(X, xxinv, Y, fac_hat, load_hat)
      betanorm <- norm(beta - betaold)
      betaold <- beta
      
      # set counter + 1
      n <- n + 1
    
    }
      
    "err <- Y - fac_hat %*% t(load_hat)
    for (n in 1:length(param)) {
      err <- err - X[,,n] * beta[n]
    }
    
    sigma <- tr(err %*% t(err))
    
    sigma <- tr(err %*% t(err)) / ((i - r) * (t - r) - p)
    
    D_F <- parameter_variance(X, fac_hat, load_hat)
    D_F_diag <- diag(D_F)
    
    variance <- sigma * D_F
    sd <- sqrt(diag(variance))"
    
    return(beta)
    
  }