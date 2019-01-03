load <- matrix(rnorm(N * R), nrow = N, ncol = R)
fac <- matrix(rnorm(T * (R-1)), nrow = T, ncol = (R-1))
fac1 <- seq(from = 1, to = 1.2, length.out = T) + rnorm(T,0,0.02)
fac <- cbind(fac, fac1)

epsilon <- epsilon

X <- array(data = NA, dim = c(T, N, length(param)))

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
