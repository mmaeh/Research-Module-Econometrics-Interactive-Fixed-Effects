#load packages
library('phtt')
library('psych')
library('zeallot')

#functions used in the following
function.sources <- paste('./functions/', list.files('./functions/', pattern="*.R"), sep = '')
sapply(function.sources, source, .GlobalEnv)

#combinations of i and t to use in simulation
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

#constants used
param <- c(1, 3, 0.75)
r <- 2
m <- 1000

#container to store results
temp_results <- data.frame(matrix(nrow = m, ncol = 12))
results <- data.frame(matrix(nrow = length(combinations), ncol = 14))
colnames(results) <- c('i', 't', 
                       'coef1_intertactive', 'coef2_intertactive', 'coef3_intertactive', 
                       'sd1_interactive', 'sd2_interactive', 'sd3_interactive',
                       'coef1_within', 'coef2_within', 'coef3_within',
                       'sd1_within', 'sd2_within', 'sd3_within')


#--------------------------------------------
#start simulation process--------------------
#--------------------------------------------


progress <- txtProgressBar(min = 0, max = length(combinations), style = 3)
iter <- 0

for (c in combinations) {
  
  c(i, t) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(i, t)
  
  for (j in 1:m) {
    
    #create simulated data
    
    epsilon <- error_function(t, i, hetero = TRUE)
    source('data_generating_lagged_dep.R')
    
    #estimate naive-estimator
    
    #temp_results[j, 1:4] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    #estimate within-estimator
    
    temp_results[j, 7:9] <- within_est(X, Y, individual = TRUE, time = TRUE) #same coeff as plm
    #temp_results[j, 10:12] <- plm(Y ~ X1 + X2 + X3 - 1, data = plm_df, effect = 'twoways', model = 'within', index = c('t', 'i'))$coefficients
    
    #estimate infeasible-estimator
    
    #temp_results[j, 9:12] <- infeasible_est(X, Y, given = "factors")
    
    #estimate interactive-estimator
    
    #temp_results[j, 1:3] <- interactive_est(X, Y, r, 0.0001) #same coeff as phtt
    #temp_results[j, 4:6] <- Eup(Y ~ -1 + X[,,1] + X[,,2] + X[,,3], factor.dim = 2)$slope.para
    
  }
  
  results[iter + 1, 3:14] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 
  setTxtProgressBar(progress, iter)
  
}

write.xlsx(results, file = 'comparison_table.xlsx', sheetName = 'table_5')
