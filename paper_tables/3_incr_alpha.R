#load packages
library('phtt')
library('psych')
library('xlsx')
library('zeallot')

#functions used in the following
function.sources <- paste('./functions/', list.files('./functions/', pattern="*.R"), sep = '')
sapply(function.sources, source, .GlobalEnv)

#combinations of i and t to use in simulation
combinations <- list(
  c(100, 3),
  c(100, 5),
  c(100, 10),
  c(100, 20),
  c(100, 50),
  c(100, 100),
  c(3, 100),
  c(5, 100),
  c(10, 100),
  c(20, 100),
  c(50, 100)
)

#constants used
param <- c(1, 3)
r <- 2
n_rep <- 1000

#container to store results
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 60))
results <- data.frame(matrix(nrow = length(combinations), ncol = 62))
colnames(results) <- c('i', 't', 'alpha1_coef1_ols', 'alpha1_sd1_ols', 'alpha1_coef2_ols', 'alpha1_sd2_ols', 
                       'alpha1_coef1_within', 'alpha1_sd1_within', 'alpha1_coef2_within', 'alpha1_sd2_within',
                       'alpha1_coef1_interactive', 'alpha1_sd1_interactive', 'alpha1_coef2_interactive', 'alpha1_sd2_interactive',
                       'alpha2_coef1_ols', 'alpha2_sd1_ols', 'alpha2_coef2_ols', 'alpha2_sd2_ols', 
                       'alpha2_coef1_within', 'alpha2_sd1_within', 'alpha2_coef2_within', 'alpha2_sd2_within',
                       'alpha2_coef1_interactive', 'alpha2_sd1_interactive', 'alpha2_coef2_interactive', 'alpha2_sd2_interactive',
                       'alpha4_coef1_ols', 'alpha4_sd1_ols', 'alpha4_coef2_ols', 'alpha4_sd2_ols', 
                       'alpha4_coef1_within', 'alpha4_sd1_within', 'alpha4_coef2_within', 'alpha4_sd2_within',
                       'alpha4_coef1_interactive', 'alpha4_sd1_interactive', 'alpha4_coef2_interactive', 'alpha4_sd2_interactive',
                       'alpha8_coef1_ols', 'alpha8_sd1_ols', 'alpha8_coef2_ols', 'alpha8_sd2_ols', 
                       'alpha8_coef1_within', 'alpha8_sd1_within', 'alpha8_coef2_within', 'alpha8_sd2_within',
                       'alpha8_coef1_interactive', 'alpha8_sd1_interactive', 'alpha8_coef2_interactive', 'alpha8_sd2_interactive',
                       'alpha16_coef1_ols', 'alpha16_sd1_ols', 'alpha16_coef2_ols', 'alpha16_sd2_ols', 
                       'alpha16_coef1_within', 'alpha16_sd1_within', 'alpha16_coef2_within', 'alpha16_sd2_within',
                       'alpha16_coef1_interactive', 'alpha16_sd1_interactive', 'alpha16_coef2_interactive', 'alpha16_sd2_interactive')
                       

#--------------------------------------------
#start simulation process--------------------
#--------------------------------------------


progress <- txtProgressBar(min = 0, max = length(combinations) * n_rep, style = 3)
iter <- 0

for (c in combinations) {
  
  c(i, t) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(i, t)
  
  for (rep in 1:n_rep) {

    #create simulated data
    epsilon <- error_function(t, i, mean = 0, sd = 2)
    source('./data_generating/data_generating_alpha.R')
      
    # alpha == 1
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 1 * fac %*% t(load) + epsilon
    temp_results[rep, c(1, 3)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    temp_results[rep, c(5, 7)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    temp_results[rep, c(9, 11)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)  
    
    # alpha == 2
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 2 * fac %*% t(load) + epsilon
    temp_results[rep, c(13, 15)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    temp_results[rep, c(17, 19)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    temp_results[rep, c(21, 23)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)  
    
    # alpha == 4
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 4 * fac %*% t(load) + epsilon
    temp_results[rep, c(25, 27)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    temp_results[rep, c(29, 31)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    temp_results[rep, c(33, 35)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)  
    
    # alpha == 8
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 8 * fac %*% t(load) + epsilon
    temp_results[rep, c(37, 39)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    temp_results[rep, c(41, 43)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    temp_results[rep, c(45, 47)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)   
    
    # alpha == 16
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 16 * fac %*% t(load) + epsilon
    temp_results[rep, c(49, 51)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    temp_results[rep, c(53, 55)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    temp_results[rep, c(57, 59)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)  
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }

  for (k in seq(2, 60, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  results[iter + 1, 3:62] <- colMeans(temp_results, na.rm = TRUE)

  iter <- iter + 1 
  
}

saveRDS(results, './output/table_III.rds')
write.csv(results, './output/table_III.csv')