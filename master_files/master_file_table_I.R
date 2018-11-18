#load packages
library('phtt')
library('plm')
library('psych')
library('xlsx')
library('zeallot')

#functions used in the following
function.sources <- paste('./functions/', list.files('./functions/', pattern="*.R"), sep = '')
sapply(function.sources, source, .GlobalEnv)

#combinations of i and t to use in simulation
combinations <- list(
  c(100, 10),
  c(100, 20),
  c(100, 50),
  c(100, 100),
  c(10, 100),
  c(20, 100),
  c(50, 100)
)

#parameters and constants used
param <- c(1, 3, 5, 2, 4)
r <- 2
m <- 1000

#container to store results
temp_results <- data.frame(matrix(nrow = m, ncol = 30))
results <- data.frame(matrix(nrow = length(combinations), ncol = 32))
colnames(results) <- c('i', 't', 'coef1_ols', 'coef2_ols', 'coef3_ols', 'coef4_ols', 'coef5_ols', 
                       'sd1_ols', 'sd2_ols', 'sd4_ols', 'sd4_ols', 'sd5_ols', 
                       'coef1_infeasible', 'coef2_infeasible', 'coef3_infeasible', 'coef4_infeasible', 'coef5_infeasible', 
                       'sd1_infeasible', 'sd2_infeasible', 'sd3_infeasible', 'sd4_infeasible', 'sd5_infeasible',
                       'coef1_interactive', 'coef2_interactive', 'coef3_interactive', 'coef4_interactive', 'coef5_interactive',
                       'sd1_interactive', 'sd2_interactive', 'sd3_interactive', 'sd4_interactive', 'sd5_interactive')


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
    
    epsilon <- error_function(t, i, mean = 0, sd = 2)
    source('./data_generating/data_generating_table_I.R')
    
    #estimate naive-estimator
    
    #temp_results[j, 1:5] <- within_est(X, Y, individual = FALSE, time = FALSE)

    #estimate infeasible-estimator
    
    temp_results[j, 6:10] <- infeasible_est(X, Y, given = "factors") 
    
    #estimate interactive-estimator
    
    #temp_results[j, 6:10] <- interactive_est(X, Y, r, 0.0001) #!!Not possible to take within-estimator as a starting value!!
    #temp_results[j, 1:4] <- Eup(Y ~ X[,,1] + X[,,2] + X[,,4] + X[,,5], factor.dim = 2)$slope.para
    
  }
  
  results[iter + 1, 3:22] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 
  setTxtProgressBar(progress, iter)
  
}

write.xlsx(results, './output/table_I.xlsx')
