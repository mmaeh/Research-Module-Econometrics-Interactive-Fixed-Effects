load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t, ncol = r)

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



plm_df <- data.frame(matrix(nrow = i*t, ncol = 8))
colnames(plm_df) <- c('i', 't', 'Y', 'X1', 'X2', 'X3', 'X4', 'X5')

plm_df$i <- rep(1:i, each = t)
plm_df$t <- rep(1:t, times = i)

for (j in 1:i) {
  plm_df[(((j-1)*t+1):(j*t)), 'Y'] <- Y[,j]
  plm_df[(((j-1)*t+1):(j*t)), 'X1'] <- X[,j,1]
  plm_df[(((j-1)*t+1):(j*t)), 'X2'] <- X[,j,2]
  plm_df[(((j-1)*t+1):(j*t)), 'X3'] <- X[,j,3]
  plm_df[(((j-1)*t+1):(j*t)), 'X4'] <- X[,j,4]
  plm_df[(((j-1)*t+1):(j*t)), 'X5'] <- X[,j,5]
  
}
