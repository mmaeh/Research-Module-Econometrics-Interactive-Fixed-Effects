# Simulate factors and loadings from standard normal distribution.
load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t, ncol = r)

# Set up container to store regressors.
X <- array(data = NA, dim = c(t, i, length(param)))

# Compute regressors according to model specification in simulation part.
X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)
X[,,3] <- matrix(1, nrow = t, ncol = i)

aa <- colSums(t(load)) + matrix(rnorm(1 * i), nrow = 1, ncol = i)
bb <- rowSums(fac, dims = 1) + matrix(rnorm(1 * t), nrow = t, ncol = 1)

for (j in 1:t) {
  X[j,,4] <- aa   
}

for (j in 1:i) {
  X[,j,5] <- bb   
}

Y <- X[,,1] * param[1] + X[,,2] * param[2] + X[,,3] * param[3] + X[,,4] * param[4] + X[,,5] * param[5] + fac %*% t(load) + epsilon
