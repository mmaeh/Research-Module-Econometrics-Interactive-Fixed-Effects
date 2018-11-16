#c(i, t) %<-% c(combinations[[iter + 1]][1], combinations[[iter + 1]][2])

load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t, ncol = r)
#ones <- matrix(1, nrow = r, ncol = 1)

X <- array(data = NA, dim = c(t, i, length(param)))

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

Y <- fac %*% t(load) + epsilon

for (k in 1:length(param)) {
  Y <- Y + X[,,k] * param[k]
}
