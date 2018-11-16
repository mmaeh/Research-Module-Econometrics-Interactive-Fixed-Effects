load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t, ncol = r)

#epsilon <- matrix(rnorm(i * t, sd = 2), nrow = t, ncol = i)
epsilon <- epsilon

X <- array(data = NA, dim = c(t, i, length(param)))
Y <- matrix(0, nrow = t, ncol = i)

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = t, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t, ncol = i)

for (j in 2:t) {
  if (j == 1) {
    Y[j,] <- X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  } else {
    Y[j,] <- Y[j-1,] * param[3] + X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  }
}

X[2:t,,3] <- Y[1:(t-1),]
X <- X[2:t,,]
Y <- Y[2:t,]
