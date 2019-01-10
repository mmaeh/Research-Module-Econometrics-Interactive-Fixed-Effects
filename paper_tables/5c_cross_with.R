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

# Used constants.
param <- c(2, 4)
R <- 2
n_rep <- 1000

# Container to store results.
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 16))
results <- data.frame(matrix(nrow = length(combinations), ncol = 18))
colnames(results) <- c('i', 't', 'coef1_ols', 'sd1_ols', 'coef2_ols', 'sd2_ols', 
                       'coef1_within', 'sd1_within', 'coef2_within', 'sd2_within',
                       'coef1_infeasible', 'sd1_infeasible', 'coef2_infeasible', 'sd2_infeasible',
                       'coef1_interactive', 'sd1_interactive', 'coef2_interactive', 'sd2_interactive')


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
    
    epsilon <- error_function(t, i, mean = 0, sd = sqrt(2), cross_corr = TRUE, rho = 0.7, cross_hetero = TRUE)
    source('./data_generating/data_generating_table_III_IV.R')
    
    #estimate naive-estimator
    
    temp_results[rep, c(1, 3)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    #estimate within-estimator
    
    temp_results[rep, c(5, 7)] <- within_est(X, Y, individual = TRUE, time = TRUE) #coeff are right!
    
    #estimate infeasible-estimator
    
    temp_results[rep, c(9, 11)] <- infeasible_est(X, Y, given = "factors") #coeff seem right as well
    
    #estimate interactive-estimator
    
    temp_results[rep, c(13, 15)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }
  
  for (k in seq(2, 16, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  results[iter + 1, 3:18] <- colMeans(temp_results, na.rm = TRUE)  
  
  iter <- iter + 1 
  
}

saveRDS(results, './output/table_Vc.rds')
write.xlsx(results, './output/table_Vc.xlsx')

table1 <- xtable(results, digits = 3, auto = TRUE)
print.xtable(table1)
