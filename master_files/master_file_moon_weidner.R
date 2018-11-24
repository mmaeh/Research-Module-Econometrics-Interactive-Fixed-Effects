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
  c(100, 10),
  c(100, 30),
  c(100, 200),
  c(100, 300),
  c(300, 10),
  c(300, 30),
  c(300, 200),
  c(300, 300)
)

#constants used
param <- 1
r <- 2
n_rep <- 1000

#container to store results
temp_results <- data.frame(matrix(nrow = m, ncol = 12))
results <- data.frame(matrix(nrow = length(combinations), ncol = 14))
colnames(results) <- c('i', 't', 
                       'R0', 'SD_R0', 'R1', 'SD_R1', 'R2', 'SD_R2',
                       'R3', 'SD_R3', 'R4', 'SD_R4', 'R5', 'SD_R5'
                       )


#--------------------------------------------
#start simulation process--------------------
#--------------------------------------------


progress <- txtProgressBar(min = 0, max = length(combinations), style = 3)
iter <- 0

for (c in combinations) {
  
  c(i, t) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(i, t)
  
  for (rep in 1:n_rep) {
    
    #create simulated data
    
    source('./data_generating/data_generating_moon_weidner.R')
    
    #estimate interactive-estimator from R = 0 to R = 5
     
    temp_results[rep, 1] <- within_est(X, Y)
    temp_results[rep, 3] <- interactive_est_2(X, Y, 1, beta_start = "OLS", 0.0001)  
    temp_results[rep, 5] <- interactive_est_2(X, Y, 2, beta_start = "OLS", 0.0001)  
    temp_results[rep, 7] <- interactive_est_2(X, Y, 3, beta_start = "OLS", 0.0001)  
    temp_results[rep, 9] <- interactive_est_2(X, Y, 4, beta_start = "OLS", 0.0001)  
    temp_results[rep, 11] <- interactive_est_2(X, Y, 5, beta_start = "OLS", 0.0001)  
    
  }
  
  for (k in c(2, 4, 6, 8, 10, 12)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  results[iter + 1, 3:14] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 
  setTxtProgressBar(progress, iter)
  
}

