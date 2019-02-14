# The following code reproduces Table A.3 of the paper.
# The dependent variable is simulated according to:
# Y_{it} = X_{it,1} \beta_{1} + X_{it,2} \beta_{2} + Y_{it-1} \beta_{3} + \lambda_{i}' F_{t} + \varepsilon_{it}
# with serial-correlated and hetroskedastic errors.

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
colnames(results) <- c('N', 'T', 
                       'coef1_ols', 'sd1_ols', 'coef2_ols', 'sd2_ols', 'coef3_ols', 'sd3_ols', 
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
  
  c(N, T) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(N, T)
  
  for (rep in 1:n_rep) {
    
    # Set burn_in to 100 for simulation of lagged dep. variable.
    burn_in <- 100
    
    # Create error matrix from normal distribution.
    epsilon <- error_function(T + burn_in, N, mean = 0, sd = 2)
    
    # Create simulated data.
    source('./mc_simulations/data_generating/data_generating_lagged_dep.R')
    
    # Estimate via naive OLS estimator. 
    temp_results[rep, c(1, 3, 5)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    
    # Estimate via within-estimator.
    temp_results[rep, c(7, 9, 11)] <- within_est(X, Y, individual = TRUE, time = TRUE)

    # Estimate via infeasible-estimator.
    temp_results[rep, c(13, 15, 17)] <- infeasible_est(X, Y, given = "factors")
    
    # Estimate via IFE estimator.
    temp_results[rep, c(19, 21, 23)] <- interactive_est_3(X, Y, R, tolerate = 0.0001)

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

# Save results.
saveRDS(results, './mc_simulations/output/table_lagged_dep.rds')
write.xlsx(results, './mc_simulations/output/table_lagged_dep.xlsx')

# Print output to LaTeX format.
table <- xtable(results[,c(1:2, 15:26)], digits = 3, auto = TRUE)
print.xtable(table)
