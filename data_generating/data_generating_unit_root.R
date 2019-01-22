# Set burn-in to 100.
burn_in = 100

# Simulate loadings from standard normal distribution.
load <- matrix(rnorm(N * R), nrow = N, ncol = R)

# Simulate first factor according to unit root process.
fac1 <- matrix(NA, nrow = T + burn_in, ncol = 1)
fac1[1,1] <- rnorm(1)
for (i in 2:(T + burn_in)) {
  fac1[i, 1] <- fac1[i-1, 1] + rnorm(1) 
}

# Simulate second factor from standard normal distribution.
fac2 <- matrix(rnorm(T * (R-1)), nrow = T, ncol = (R-1))

# Bind factors together.
fac <- cbind(fac1[(burn_in + 1):(burn_in + T), 1], fac2)

# Set up container to store regressors.
X <- array(data = NA, dim = c(T, N, length(param)))

# Compute regressors according to model specification in simulation part.
X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

# Accordingly compute dependent variable.
Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
