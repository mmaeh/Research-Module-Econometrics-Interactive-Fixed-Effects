burn_in <- 100

load <- matrix(rnorm(i * r), nrow = i, ncol = r)
fac <- matrix(rnorm(t * r), nrow = t + burn_in, ncol = r)

#epsilon <- matrix(rnorm(i * t, sd = 2), nrow = t, ncol = i)
epsilon <- epsilon

X <- array(data = NA, dim = c(t + burn_in, i, length(param)))
Y <- matrix(0, nrow = t + burn_in, ncol = i)

X[,,1] <- 1 + fac %*% t(load) + matrix(1, nrow = t + burn_in, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t + burn_in, ncol = i)
X[,,2] <- 1 + fac %*% t(load) + matrix(1, nrow = t + burn_in, ncol = r) %*% t(load) + fac %*% matrix(1, nrow = r, ncol = i) + matrix(rnorm(t * i), nrow = t + burn_in, ncol = i)



for (j in 1:(t+burn_in)) {
  if (j == 1) {
    Y[j,] <- X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  } else {
    Y[j,] <- Y[j-1,] * param[3] + X[j,,1] * param[1] + X[j,,2] * param[2] + (fac %*% t(load))[j,] + epsilon[j,]
  }
}

X[burn_in:(burn_in + t),,3] <- Y[(burn_in - 1):(t + burn_in - 1),]
X <- X[(burn_in + 1):(t + burn_in),,]
Y <- Y[(burn_in + 1):(t + burn_in),]

"plm_df <- data.frame(matrix(nrow = i*(t-1), ncol = 6))
colnames(plm_df) <- c('i', 't', 'Y', 'X1', 'X2', 'X3')

plm_df$i <- rep(1:i, each = (t-1))
plm_df$t <- rep(1:(t-1), times = i)

for (j in 1:i) {
  plm_df[((j-1)*t-(j-2)):(j*(t-1)), 'Y'] <- Y[,j]
  plm_df[((j-1)*t-(j-2)):(j*(t-1)), 'X1'] <- X[,j,1]
  plm_df[((j-1)*t-(j-2)):(j*(t-1)), 'X2'] <- X[,j,2]
  plm_df[((j-1)*t-(j-2)):(j*(t-1)), 'X3'] <- X[,j,3]
  }"
