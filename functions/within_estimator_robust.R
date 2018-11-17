within_est_2 <- function(X,
                       Y,
                       individual = FALSE,
                       time = FALSE) {
  c(t, i, p) %<-% dim(X)
  xx <- matrix(0, nrow = p, ncol = p)
  xy <- matrix(0, nrow = p, ncol = 1)
  X_dem <- array(data = NA, dim = c(t, i, length(param)))
  
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
  
  err <- Y
  for (j in 1:length(param)) {
    err <- err - X[,,j] * beta[j]
  }
  
  for (j in 1:t) {
    middle_term <- t(X_dem[j,,]) %*% err[j,] %*% t(err[j,]) %*% X_dem[j,,] 
  }
  
  var <- solve(xx) %*% middle_term %*% solve(xx)
  sd <- sqrt(diag(var))
  
  return(c(beta, sd))
  
}