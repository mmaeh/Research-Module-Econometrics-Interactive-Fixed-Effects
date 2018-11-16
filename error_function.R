error_function <-
  function(t,
           i,
           mean = 0,
           sd = 1,
           cross_corr = FALSE,
           serial_corr = FALSE,
           rho = NA,
           hetero = FALSE) {
    
    epsilon <- matrix(0, nrow = t, ncol = i)
    
    if ((cross_corr | serial_corr == TRUE) & (is.na(rho) == TRUE)) {
      stop('You need to specify rho.')
    } else if (cross_corr == TRUE) {
      for (k in 1:i) {
        if (k == 1) {
          epsilon[, k] <- rnorm(t, mean = mean, sd = sd)
        } else {
          epsilon[, k] <-
            rho * epsilon[, k - 1] + rnorm(t, mean = mean, sd = sd)
        }
      }
    } else if (serial_corr == TRUE) {
      for (k in 1:t) {
        if (k == 1) {
          epsilon[k, ] <- rnorm(i, mean = mean, sd = sd)
        } else {
          epsilon[k, ] <-
            rho * epsilon[k - 1, ] + rnorm(i, mean = mean, sd = sd)
        }
      }
    } else if (cross_corr == TRUE & serial_corr == TRUE) {
      epsilon[1, ] <- rnorm(i, mean = mean, sd = sd)
      epsilon[, 1] <- rnorm(t, mean = mean, sd = sd)
      for (k in 2:t) {
        for (j in 2:i) {
          epsilon[k, j] <-
            rho * epsilon[k - 1, j - 1] + rnorm(mean = mean, sd = sd)
        }
      }
    } else if (hetero == TRUE) {
      odd <- seq(from = 1, to = t, by = 2)
      even <- seq(from = 2, to = t, by = 2)
      
      epsilon[odd, ] <- rnorm(length(odd) * i, mean = 0, sd = 1)
      epsilon[even, ] <- rnorm(length(even) * i, mean = 0, sd = 2)
    } else {
      epsilon <- rnorm(t * i, mean = mean, sd = sd)
    }
    
    
    return(epsilon)
    
  }
