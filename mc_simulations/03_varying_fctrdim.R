# The following code reproduces Table 3 of the paper.
# The dependent variable is simulated according to:
# Y_{it} = X_{it,1} \beta_{1} + X_{it,2} \beta_{2} + \alpha \lambda_{i}' F_{t} + \varepsilon_{it}
# with R from {01,2,3,4,5}.

# Load necessary packages.
library(phtt)
library(psych)
library(xlsx)
library(xtable)
library(zeallot)

# Load necessary functions used in the simulation.
setwd('./mc_simulations/')
function.sources <- paste('./functions/', list.files('./functions/', pattern="*.R"), sep = '')
sapply(function.sources, source, .GlobalEnv)

# Combinations of N and T used in the simulation.
combinations <- list(
  c(100, 5),
  c(100, 10),
  c(100, 20),
  c(100, 50),
  c(100, 100),
  c(5, 100),
  c(10, 100),
  c(20, 100),
  c(50, 100)
)

# Used constants.
param <- c(1, 3)
R <- 2
n_rep <- 1000

# Container to store results.
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 24))
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
# start simulation process-------------------
#--------------------------------------------

# Set up progress bar.
progress <- txtProgressBar(min = 0, max = length(combinations) * n_rep, style = 3)
iter <- 0

# Start iteration process.
for (c in combinations) {
  
  # N and T to use.
  c(N, T) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(N, T)
  
  for (rep in 1:n_rep) {
    
    # Create error matrix from normal distribution with variance 4.
    epsilon <- error_function(T, N, mean = 0, sd = 2)
    
    # Create simulated data, regressors and dependent variable.
    source('./data_generating/data_generating_general.R')
    
    # Estimate IFE estimator for R = 0 to R = 5.
    temp_results[rep, c(1, 3)] <- within_est(X, Y) # R = 0 ist just OLS.
    temp_results[rep, c(5, 7)] <- interactive_est_3(X, Y, R = 1, tolerate = 0.0001)  
    temp_results[rep, c(9, 11)] <- interactive_est_3(X, Y, R = 2, tolerate = 0.0001)  
    temp_results[rep, c(13, 15)] <- interactive_est_3(X, Y, R = 3, tolerate = 0.0001)  
    temp_results[rep, c(17, 19)] <- interactive_est_3(X, Y, R = 4, tolerate = 0.0001)  
    temp_results[rep, c(21, 23)] <- interactive_est_3(X, Y, R = 5, tolerate = 0.0001)  
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }
  
  # Compute standard errors corresponding to N, T combination. 
  for (k in seq(2, 24, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  # Store estimates and standard errors in results table.
  results[iter + 1, 3:26] <- colMeans(temp_results, na.rm = TRUE)  
  
  # Set counter plus one.
  iter <- iter + 1 

}

# Save results table.
saveRDS(results, './output_tables/table_03.rds')
write.csv(results, './output_tables/table_03.csv')

# Print output to LaTeX format.
# table <- xtable(results, digits = 3, auto = TRUE)
# print.xtable(table, include.rownames = FALSE)
