# Simulate factors and loadings from standard normal distribution.
load_random <- matrix(rnorm(N), nrow = N, ncol = 1)
fac_random <- matrix(rnorm(T), nrow = T, ncol = 1)

load_const <- matrix(1, nrow = N, ncol = 1)
fac_const <- matrix(1, nrow = T, ncol = 1)

load <- cbind(load_random, load_const)
fac <- cbind(fac_const, fac_random)

# Set up container to store regressors.
X <- array(data = NA, dim = c(T, N, length(param)))

# Compute regressors according to model specification in simulation part.
X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

# Accordingly compute dependent variable.
Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
