within_est <- function(X,
                       Y,
                       individual = FALSE,
                       time = FALSE) {
  c(t, i, p) %<-% dim(X)
  
  xx <- matrix(0, nrow = p, ncol = p)
  xy <- matrix(0, nrow = p, ncol = 1)
  X_dem <- array(data = NA, dim = c(t, i, p))
  
  M_i <- diag(1, i)
  M_t <- diag(1, t)
  
  if (individual == TRUE) {
    M_i <-
      diag(1, i) - ones(i) %*% solve(t(ones(i)) %*% ones(i)) %*% t(ones(i))
  }
  
  if (time == TRUE) {
    M_t <-
      diag(1, t) - ones(t) %*% solve(t(ones(t)) %*% ones(t)) %*% t(ones(t))
  }
  
  for (j in 1:p) {
    for (k in j:p) {
      x_j_fe <- M_t %*% X[, , j] %*% M_i
      x_k_fe <- M_t %*% X[, , k] %*% M_i
      X_dem[,,j] <- x_j_fe
      xx[j, k] <- tr(t(x_j_fe) %*% x_k_fe)
      xx[k, j] <- xx[j, k]
      
      xy[j] <- tr(t(x_j_fe) %*% Y)
    }
  }
  
  beta <- solve(xx) %*% xy
  
  "err <- M_t %*% Y %*% M_i
  for (j in 1:p) {
    err <- err - X_dem[,,j] * beta[j]
  }
  
  #sigma <- tr(err %*% t(err)) / (i * t - t - i - p) 
  sigma <- tr(err %*% t(err)) / (i * t - p)
  
  var_beta <- sigma * solve(xx)
  sd_beta <- sqrt(diag(var_beta))"
  
  return(beta)
  
}