burn_in = 100

load <- matrix(rnorm(N * R), nrow = N, ncol = R)
fac1 <- matrix(rnorm(T * (R-1)), nrow = T, ncol = (R-1))

fac2 <- matrix(NA, nrow = T + burn_in, ncol = 1)
fac2[1,1] <- rnorm(1)
for (i in 2:(T + burn_in)) {
  fac2[i, 1] <- fac2[i-1, 1] + rnorm(1) 
}

fac <- cbind(fac1, fac2[(burn_in + 1):(burn_in + T), 1])

epsilon <- epsilon

X <- array(data = NA, dim = c(T, N, length(param)))

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
