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
temp_results <- data.frame(matrix(nrow = m, ncol = 24))
results <- data.frame(matrix(nrow = length(combinations), ncol = 26))
colnames(results) <- c('i', 't', 
                       'R0_coef1', 'R0_sd1', 'R0_coef2', 'R0_sd2',
                       'R1_coef1', 'R1_sd1', 'R1_coef2', 'R1_sd2',
                       'R2_coef1', 'R2_sd1', 'R2_coef2', 'R2_sd2',
                       'R3_coef1', 'R3_sd1', 'R3_coef2', 'R3_sd2',
                       'R4_coef1', 'R4_sd1', 'R4_coef2', 'R4_sd2',
                       'R5_coef1', 'R5_sd1', 'R5_coef2', 'R5_sd2'
                       )


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
    source('./data_generating/data_generating_table_III_IV.R')
    
    #estimate interactive-estimator from R = 0 to R = 5
     
    temp_results[rep, c(1, 3)] <- within_est(X, Y)
    temp_results[rep, c(5, 7)] <- interactive_est_2(X, Y, 1, beta_start = "OLS", 0.0001)  
    temp_results[rep, c(9, 11)] <- interactive_est_2(X, Y, 2, beta_start = "OLS", 0.0001)  
    temp_results[rep, c(13, 15)] <- interactive_est_2(X, Y, 3, beta_start = "OLS", 0.0001)  
    temp_results[rep, c(17, 19)] <- interactive_est_2(X, Y, 4, beta_start = "OLS", 0.0001)  
    temp_results[rep, c(21, 23)] <- interactive_est_2(X, Y, 5, beta_start = "OLS", 0.0001)  
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }
  
  # Get standard deviations.
  for (k in seq(2, 24, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  results[iter + 1, 3:26] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 

}

saveRDS(results, './output/table_IV.rds')
write.csv(results, './output/table_IV.csv')