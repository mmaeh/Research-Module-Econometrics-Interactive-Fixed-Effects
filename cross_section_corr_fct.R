i <- 10000
t <- 10

rho <- 0.7

epsilon <- matrix(i * t, nrow = t, ncol = i)

for (k in 1:i) {
  if (k == 1) {
    epsilon[, k] <- rnorm(t, mean = 0, sd = sqrt(2))
  }
  else {
    epsilon[, k] <-
      rho * epsilon[, k - 1] + rnorm(t, mean = 0, sd = sqrt(2))
  }
}
