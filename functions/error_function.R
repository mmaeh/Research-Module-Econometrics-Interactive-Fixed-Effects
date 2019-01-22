error_function <-
  function(T,
           N,
           mean = 0, # Deafault mean.
           sd = 1, # Default Standard Error.
           burn_in = 0, # Burn-in, which will be disregarded in the end.
           cross_corr = FALSE, # Default no cross or serial correlation.
           serial_corr = FALSE,
           rho = NA, # AR(1) parameter.
           cross_hetero = FALSE, # Default no hetroskedasticity.
           serial_hetero = FALSE) {
    
    # Default status is error matrix with iid errors.
    epsilon <- matrix(rnorm(T * N, mean = mean, sd = sd), nrow = T, ncol = N)
    
    # Warning that AR parameter rho needs to be specified, when using either
    # cross- or serial-correlation.
    if ((cross_corr | serial_corr == TRUE) & (is.na(rho) == TRUE)) {
      stop('You need to specify rho.')
    }
    
    # Generate cross-correlated errors, when needed.   
    if (cross_corr == TRUE) {
      epsilon <- matrix(0, nrow = T, ncol = N + burn_in)
      
      # Simulate AR(1) process, with model parameter rho 
      # across individual dimension, i.e. cross-correlation. 
      for (i in 1:(N + burn_in)) {
        if (i == 1) { # First observation.
          epsilon[, k] <- rnorm(T, mean = mean, sd = sd)
        } else {
          epsilon[, i] <-
            rho * epsilon[, i - 1] + rnorm(T, mean = mean, sd = sd)
        }
      }

      # Disregard initial burn-in.      
      epsilon <- epsilon[, (burn_in + 1):(burn_in + N)]
      
    }
    
    # Generate serial-correlated errors, when needed.  
    if (serial_corr == TRUE) {
      epsilon <- matrix(0, nrow = (T + burn_in), ncol = N)
      
      # Simulate AR(1) process, with model parameter rho 
      # across time dimension, i.e. serial-correlation.
      for (t in 1:(T + burn_in)) {
        if (t == 1) { # First observation.
          epsilon[t, ] <- rnorm(N, mean = mean, sd = sd)
        } else {
          epsilon[t, ] <-
            rho * epsilon[k - 1, ] + rnorm(N, mean = mean, sd = sd)
        }
      }
      
      # Disregard initial burn-in.
      epsilon <- epsilon[(burn_in + 1):(burn_in + T),]
      
    }
    
    # If cross_hetero == TRUE (serial_hetero == TRUE), even rows (columns) 
    # have a 2*times greater variance than odd ones. This also works in 
    # combination with cross- serial-correlated errors.
    
    if (serial_hetero == TRUE) {
      # Select even rows across serial-dimension.
      even <- seq(from = 2, to = T, by = 2)
      # Double variance.
      epsilon[even, ] <- sqrt(2) * epsilon[even, ]
    }
    
    if (cross_hetero == TRUE) {
      # Select even rows across cross-dimension.
      even <- seq(from = 2, to = N, by = 2)
      # Double variance.
      epsilon[, even] <- sqrt(2) * epsilon[, even]
    }
    
    return(epsilon)
    
  }
