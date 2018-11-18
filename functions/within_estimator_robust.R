within_est_2 <- function(X,
                       Y,
                       individual = FALSE,
                       time = FALSE) {

# dimension of X
  
  c(t, i, p) %<-% dim(X)

# containers need for storing
  
  xx <- matrix(0, nrow = p, ncol = p)
  xy <- matrix(0, nrow = p, ncol = 1)
  X_dem <- array(data = NA, dim = c(t, i, length(param)))
  
  M_i <- diag(1, i)
  M_t <- diag(1, t)

# Projection matrices  
    
  if (individual == TRUE) {
    M_i <-
      diag(1, i) - ones(i) %*% solve(t(ones(i)) %*% ones(i)) %*% t(ones(i))
  }
  
  if (time == TRUE) {
    M_t <-
      diag(1, t) - ones(t) %*% solve(t(ones(t)) %*% ones(t)) %*% t(ones(t))
  }

# Demean regressors  
    
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

# Get beta estimates from ols applied to demeaned values  
    
  beta <- solve(xx) %*% xy

# Estimate parameter variance
      
  err <- M_t %*% Y %*% M_i
  for (j in 1:p) {
    err <- err - X_dem[,,j] * beta[j]
  }
  
  
  for (j in 1:t) {
    sandwich <- t(X_dem[j,,]) %*% (err[j,] %*% t(err[j,]) / (t*i - t - i - p)) %*% X_dem[j,,] #df adjustment?
  }
  
  var_beta <- solve(xx) %*% sandwich %*% solve(xx)
  sd_beta <- sqrt(diag(var_beta))
 
   
  return(c(beta, sd_beta))
  
}