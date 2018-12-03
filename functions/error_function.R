error_function <-
  function(t,
           i,
           mean = 0,
           sd = 1,
           burn_in = 0,
           cross_corr = FALSE,
           serial_corr = FALSE,
           rho = NA,
           cross_hetero = FALSE,
           serial_hetero = FALSE) {
    # container matrix
    epsilon <- epsilon <- rnorm(t * i, mean = mean, sd = sd)
    
    # warning to specify AR paramter
    if ((cross_corr | serial_corr == TRUE) & (is.na(rho) == TRUE)) {
      stop('You need to specify rho.')
    }
    
    # Cross correlated errors   
    if (cross_corr == TRUE) {
      epsilon <- matrix(0, nrow = t, ncol = i + burn_in)
      
      for (k in 1:(i + burn_in)) {
        if (k == 1) {
          epsilon[, k] <- rnorm(t, mean = mean, sd = sd)
        } else {
          epsilon[, k] <-
            rho * epsilon[, k - 1] + rnorm(t, mean = mean, sd = sd)
        }
      }
    }
    
    # Serial correlated errors
    if (serial_corr == TRUE) {
      epsilon <- matrix(0, nrow = (t + burn_in), ncol = i)
      
      for (k in 1:(t + burn_in)) {
        if (k == 1) {
          epsilon[k, ] <- rnorm(i, mean = mean, sd = sd)
        } else {
          epsilon[k, ] <-
            rho * epsilon[k - 1, ] + rnorm(i, mean = mean, sd = sd)
        }
      }
    }
    
    # if hetero == TRUE even rows / columns have a sd 2*times greater than the odd ones
    if (serial_hetero == TRUE) {
      even <- seq(from = 2, to = t, by = 2)
      epsilon[even, ] <- sqrt(2) * epsilon[even, ]
    }
    
    if (cross_hetero == TRUE) {
      even <- seq(from = 2, to = i, by = 2)
      epsilon[, even] <- sqrt(2) * epsilon[, even]
    }
    
    return(epsilon[1:t, 1:i])
    
  }



"else if (cross_corr == TRUE & serial_corr == TRUE) {
epsilon[1, ] <- rnorm(i, mean = mean, sd = sd)
epsilon[, 1] <- rnorm(t, mean = mean, sd = sd)
for (k in 2:t) {
for (j in 2:i) {
epsilon[k, j] <-
rho * epsilon[k - 1, j - 1] + rnorm(mean = mean, sd = s"

"if (hetero == TRUE) {
odd <- seq(from = 1, to = t, by = 2)
even <- seq(from = 2, to = t, by = 2)

epsilon[odd,] <- rnorm(length(odd) * i, mean = 0, sd = 1)
epsilon[even,] <- epsilon[even,] rnorm(length(even) * i, mean = 0, sd = 2)
}"