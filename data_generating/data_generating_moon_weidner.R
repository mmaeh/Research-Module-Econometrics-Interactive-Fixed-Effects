"load <- matrix(rnorm(i * r, mean = 1, sd = 1), nrow = i, ncol = r)
chi <- matrix(rnorm(i * r, mean = 1, sd = 1), nrow = i, ncol = r)

fac <- matrix(rnorm((t + 1) * r, mean = 0, sd = 1), nrow = t + 1, ncol = r)
X_tilde <- matrix(rnorm(i * t, mean = 0, sd = 1), nrow = t, ncol = i)

sum_part_1 <- (fac[2:(t+1), 1] + fac[1:t, 1]) %*% t(load[, 1] + chi[, 1])
sum_part_2 <- (fac[2:(t+1), 2] + fac[1:t, 2]) %*% t(load[, 2] + chi[, 2])

X <- array(data = NA, dim = c(t, i, length(param)))

X[,,1] <- 1 + X_tilde + sum_part_1 + sum_part_2

t_error <- matrix(rt(i * (t+1), 5), nrow = t + 1, ncol = i)
Y_error <- 1 / sqrt(2) * (t_error[2:(t + 1),] + t_error[1:t])

Y <- X[,,1] * param[1] + fac[2:(t+1),] %*% t(load) + Y_error"


lambda = 1 + matrix(rnorm(i * r), nrow = i, ncol = r)
lambdaX  = 1 + matrix(rnorm(i * r), nrow = i, ncol = r)
ff = matrix(rnorm((t + 1) * r), nrow = t + 1, ncol = r)        
f = ff[2:(t+1),]
f1 = ff[1:t,]
fX = f1

v = matrix(rt(i * (t + 1), 5), nrow = t + 1, ncol = i)
v0 = v[2:(t + 1),] 
v1 = v[1:t] 
e = 1 / sqrt(2) * (v0 + v1)

X <- array(data = NA, dim = c(t, i, length(param)))
X[,,1] = 1 + matrix(rnorm(i * t), nrow = t, ncol = i) + (f + fX) %*% t(lambda + lambdaX)
Y = X[,,1] * param[1] + f %*% t(lambda) + e

