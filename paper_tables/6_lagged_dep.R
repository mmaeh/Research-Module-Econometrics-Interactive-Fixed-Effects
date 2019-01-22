# Load necessary packages.
library(phtt)
library(psych)
library(xlsx)
library(xtable)
library(zeallot)

# Load necessary functions used in the simulation.
function.sources <- paste('./functions/', list.files('./functions/', pattern="*.R"), sep = '')
sapply(function.sources, source, .GlobalEnv)

# Combinations of N and T used in the simulation.
combinations <- list(
  c(100, 5),
  c(100, 10),
  c(100, 20),
  c(100, 30),
  c(100, 50),
  c(100, 100),
  c(5, 100),
  c(10, 100),
  c(20, 100),
  c(30, 100),
  c(50, 100)
)

# Used constants.
param <- c(1, 3, 0.75)
R <- 2
n_rep <- 1000

# Container to store results.
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 24))
results <- data.frame(matrix(nrow = length(combinations), ncol = 26))
colnames(results) <- c('i', 't', 'coef1_ols', 'sd1_ols', 'coef2_ols', 'sd2_ols', 'coef3_ols', 'sd3_ols', 
                       'coef1_within', 'sd1_within', 'coef2_within', 'sd2_within', 'coef3_within', 'sd3_within',
                       'coef1_infeasible', 'sd1_infeasible', 'coef2_infeasible', 'sd2_infeasible', 'coef3_infeasible', 'sd3_infeasible',
                       'coef1_interactive', 'sd1_interactive', 'coef2_interactive', 'sd2_interactive', 'coef3_interactive', 'sd3_interactive')


#--------------------------------------------
# Start simulation process-------------------
#--------------------------------------------

# Set up progress bar.
progress <- txtProgressBar(min = 0, max = length(combinations) * n_rep, style = 3)
iter <- 0

for (c in combinations) {
  
  c(i, t) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(i, t)
  
  for (rep in 1:n_rep) {
    
    #create simulated data
    burn_in <- 100
    
    epsilon <- error_function(t + burn_in, i) #remove hetero??
    source('./data_generating/data_generating_table_V.R')
    
    #estimate naive-estimator
    
    temp_results[rep, c(1, 3, 5)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    #estimate within-estimator
    
    temp_results[rep, c(7, 9, 11)] <- within_est(X, Y, individual = TRUE, time = TRUE) #same coeff as plm

    #estimate infeasible-estimator
    
    temp_results[rep, c(13, 15, 17)] <- infeasible_est(X, Y, given = "factors")
    
    #estimate interactive-estimator
    
    temp_results[rep, c(19, 21, 23)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001) #same coeff as phtt

    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }
  
  # Get standard deviations.
  for (k in seq(2, 24, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  results[iter + 1, 3:26] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 

}

saveRDS(results, './output/table_VI.rds')
write.xlsx(results, './output/table_VI.xlsx')

table <- xtable(results[,c(1:2, 15:26)], digits = 3, auto = TRUE)
print.xtable(table)
