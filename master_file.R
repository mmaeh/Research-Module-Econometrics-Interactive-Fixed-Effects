#load packages
library('psych')
library('zeallot')

#functions used in the following
source('beta_update.R')
source('compute_factor_estimates.R')
source('compute_xx_inv.R')
source('error_function.R')
source('infeasible_estimator.R')
source('iterative_interactive_estimator.R')
source('ones_function.R')
#source('parameter_variance.R')
source('parameter_variance_2.R')
source('within_estimator.R')

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
m <- 1000

#container to store results
temp_results <- data.frame(matrix(nrow = m, ncol = 22))
results <- data.frame(matrix(nrow = length(combinations), ncol = 22))
colnames(results) <- c('coef1_ols', 'coef2_ols', 'sd1_ols', 'sd2_ols', 
                      'coef1_within', 'coef2_within', 'sd1_within', 'sd2_within',
                      'coef1_infeasible', 'coef2_infeasible', 'sd1_infeasible', 'sd2_infeasible', 'sigma_infeasible', 'df1_inf', 'df2_inf',
                      'coef1_interactive', 'coef2_interactive', 'sd1_interactive', 'sd2_interactive', 'sigma_interactive', 'df1_int', 'df2_int')


#--------------------------------------------
#start simulation process--------------------
#--------------------------------------------


progress <- txtProgressBar(min = 0, max = length(combinations), style = 3)
iter <- 0

for (c in combinations) {
  for (j in 1:m) {
    
    #create simulated data
    
    source('data_generating.R')
    
    #estimate naive-estimator
    
    temp_results[j, 1:4] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    #estimate within-estimator
    
    temp_results[j, 5:8] <- within_est(X, Y, individual = TRUE, time = TRUE)
    
    #estimate infeasible-estimator
    
    temp_results[j, 9:15] <- infeasible_est(X, Y, given = "factors")
    
    #estimate interactive-estimator
  
    temp_results[j, 16:22] <- interactive_est(X, Y, r, 0.0001)
      
  }

results[iter + 1,] <- colMeans(temp_results, na.rm = TRUE)  
    
iter <- iter + 1 
setTxtProgressBar(progress, iter)

}
