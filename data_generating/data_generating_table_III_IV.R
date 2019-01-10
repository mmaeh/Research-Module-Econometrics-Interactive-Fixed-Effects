# Simulate factors and loadings from standard normal distribution.
load <- matrix(rnorm(N * R), nrow = N, ncol = R)
fac <- matrix(rnorm(T * R), nrow = T, ncol = R)

# Set up container to store regressors.
X <- array(data = NA, dim = c(T, N, length(param)))

# Compute regressors according to model specification in simulation part.
X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T, ncol = N)

# Accordingly compute dependent variable.
Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
