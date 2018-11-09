c(i, t) %<-% c(combinations[[iter + 1]][1], combinations[[iter + 1]][2])

load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t, ncol = r)

#epsilon <- matrix(rnorm(i * t, sd = 2), nrow = t, ncol = i)
epsilon <- error_function(t, i, sd = 2)

X <- array(data = NA, dim = c(t, i, length(param)))

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)

Y <- X[,,1] * param[1] + X[,,2] * param[2] + fac %*% t(load) + epsilon