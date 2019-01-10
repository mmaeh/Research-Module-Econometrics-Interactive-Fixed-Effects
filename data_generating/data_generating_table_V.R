# Set burn_in to 100.
burn_in <- 100

# Simulate factors and loadings from standard normal distribution.
# Include additional burn-in for factor simulation.
load <- matrix(rnorm(N * R), nrow = N, ncol = R)
fac <- matrix(rnorm((T + burn_in) * R), nrow = T + burn_in, ncol = R)

# Set up container to store regressors.
X <- array(data = NA, dim = c(T + burn_in, N, length(param)))
Y <- matrix(0, nrow = T + burn_in, ncol = N)

# Compute regressors according to model specification in simulation part.
X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = T + burn_in, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T + burn_in, ncol = N)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = T + burn_in, ncol = R) %*% t(load) + fac %*% matrix(1, nrow = R, ncol = N) + matrix(rnorm(T * N), nrow = T + burn_in, ncol = N)


for (j in 1:(T + burn_in)) {
  if (j == 1) {
    Y[j,] <- X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  } else {
    Y[j,] <- Y[j-1,] * param[3] + X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  }
}

X[burn_in:(burn_in + T),,3] <- Y[(burn_in - 1):(T + burn_in - 1),]
X <- X[(burn_in + 1):(T + burn_in),,]
Y <- Y[(burn_in + 1):(T + burn_in),]
fac <- fac[(burn_in + 1):(T + burn_in),]
