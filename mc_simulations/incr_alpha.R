# The following code reproduces Table 2 of the paper.
# The dependent variable is simulated according to:
# Y_{it} = X_{it,1} \beta_{1} + X_{it,2} \beta_{2} + \alpha \lambda_{i}' F_{t} + \varepsilon_{it}
# with \alpha element {1,2,4,8}.

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
param <- c(1, 3)
R <- 2
n_rep <- 1000

# Container to store results.
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 60))
results <- data.frame(matrix(nrow = length(combinations), ncol = 62))
colnames(results) <- c('i', 't', 
                       'alpha1_coef1_ols', 'alpha1_sd1_ols', 'alpha1_coef2_ols', 'alpha1_sd2_ols', 
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

    # Simulate factors, loadings and regressors. The dependent variale
    # is explicitly computed in the following for all values of alpha.
    source('./mc_simulations/data_generating/data_generating_alpha.R')
      
    # alpha == 1
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 1 * fac %*% t(load) + epsilon
    temp_results[rep, c(1, 3)] <- within_est(X, Y, individual = FALSE, time = FALSE) # OLS
    temp_results[rep, c(5, 7)] <- within_est(X, Y, individual = TRUE, time = TRUE) # within
    temp_results[rep, c(9, 11)] <- interactive_est_3(X, Y, R, 0.0001) # IFE
    
    # alpha == 2
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 2 * fac %*% t(load) + epsilon
    temp_results[rep, c(13, 15)] <- within_est(X, Y, individual = FALSE, time = FALSE) # OLS
    temp_results[rep, c(17, 19)] <- within_est(X, Y, individual = TRUE, time = TRUE) # within
    temp_results[rep, c(21, 23)] <- interactive_est_3(X, Y, R, 0.0001) # IFE
    
    # alpha == 4
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 4 * fac %*% t(load) + epsilon
    temp_results[rep, c(25, 27)] <- within_est(X, Y, individual = FALSE, time = FALSE) # OLS
    temp_results[rep, c(29, 31)] <- within_est(X, Y, individual = TRUE, time = TRUE) # within
    temp_results[rep, c(33, 35)] <- interactive_est_3(X, Y, R, 0.0001) # IFE
    
    # alpha == 8
    Y <- X[,,1] * param[1] + X[,,2] * param[2] + 8 * fac %*% t(load) + epsilon
    temp_results[rep, c(37, 39)] <- within_est(X, Y, individual = FALSE, time = FALSE) # OLS
    temp_results[rep, c(41, 43)] <- within_est(X, Y, individual = TRUE, time = TRUE) # within
    temp_results[rep, c(45, 47)] <- interactive_est_3(X, Y, R, 0.0001) # IFE 
    
    # alpha == 16
    #Y <- X[,,1] * param[1] + X[,,2] * param[2] + 16 * fac %*% t(load) + epsilon
    #temp_results[rep, c(49, 51)] <- within_est(X, Y, individual = FALSE, time = FALSE)
    #temp_results[rep, c(53, 55)] <- within_est(X, Y, individual = TRUE, time = TRUE)
    #temp_results[rep, c(57, 59)] <- interactive_est_2(X, Y, r, beta_start = 'factor', 0.0001)  
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }

  # Compute standard errors corresponding to N, T combination. 
  for (k in seq(2, 60, 2)) {
    temp_results[, k] <- sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1]))^2))  
  }
  
  # Store estimates and standard errors in results table.
  results[iter + 1, 3:62] <- colMeans(temp_results, na.rm = TRUE)

  # Set counter plus one.
  iter <- iter + 1 
  
}

# Save results table.
saveRDS(results, './mc_simulations/output/table_incr_alpha.rds')
write.csv(results, './mc_simulations/output/table_incr_alpha.csv')

# Print output to LaTeX format.
table <- xtable(results, digits = 3, auto = TRUE)
print.xtable(table, include.rownames = FALSE)
