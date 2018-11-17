load_random <- matrix(rnorm(i), nrow = i, ncol = 1)
fac_random <- matrix(rnorm(t), nrow = t, ncol = 1)

load_const <- matrix(1, nrow = i, ncol = 1)
fac_const <- matrix(1, nrow = t, ncol = 1)

load <- cbind(load_random, load_const)
fac <- cbind(fac_const, fac_random)

X <- array(data = NA, dim = c(t, i, length(param)))

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)

Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon
