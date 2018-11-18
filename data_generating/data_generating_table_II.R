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


plm_df <- data.frame(matrix(nrow = i*t, ncol = 5))
colnames(plm_df) <- c('i', 't', 'Y', 'X1', 'X2')

plm_df$i <- rep(1:i, each = t)
plm_df$t <- rep(1:t, times = i)

for (j in 1:i) {
  plm_df[(((j-1)*t+1):(j*t)), 'Y'] <- Y[,j]
  plm_df[(((j-1)*t+1):(j*t)), 'X1'] <- X[,j,1]
  plm_df[(((j-1)*t+1):(j*t)), 'X2'] <- X[,j,2]
}