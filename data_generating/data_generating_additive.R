load <- matrix(1, nrow = N, ncol = R)
load[, 1] <- rnorm(N) 
fac <- matrix(1, nrow = T, ncol = R)
fac[, 2] <- rnorm(T) 

epsilon <- epsilon

X <- array(data = NA, dim = c(T, N, length(param)))

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
