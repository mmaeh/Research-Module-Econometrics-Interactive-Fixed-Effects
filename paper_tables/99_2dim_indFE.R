#load packages
library('phtt')
library('psych')
library('xlsx')
library('xtable')
library('zeallot')

#functions used in the following
function.sources <-
  paste('./functions/',
        list.files('./functions/', pattern = "*.R"),
        sep = '')
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
R <- 3
n_rep <- 200

#container to store results
temp_results <- data.frame(matrix(nrow = n_rep, ncol = 16))
results <-
  data.frame(matrix(nrow = length(combinations), ncol = 18))
colnames(results) <-
  c(
    'N',
    'T',
    'coef1_ols',
    'sd1_ols',
    'coef2_ols',
    'sd2_ols',
    'coef1_within',
    'sd1_within',
    'coef2_within',
    'sd2_within',
    'coef1_IFE_dim3',
    'sd1_IFE_dim3',
    'coef2_IFE_dim3',
    'sd2_IFE_dim3',
    'coef1_IFE_dim2_indFE',
    'sd1_IFE_dim2_indFE',
    'coef2_IFE_dim2_indFE',
    'sd2_IFE_dim2_indFE'
  )


#--------------------------------------------
#start simulation process--------------------
#--------------------------------------------


progress <-
  txtProgressBar(min = 0,
                 max = length(combinations) * n_rep,
                 style = 3)
iter <- 0

for (c in combinations) {
  c(N, T) %<-% combinations[[iter + 1]]
  results[iter + 1, 1:2] %<-% c(N, T)
  
  for (rep in 1:n_rep) {
    #create simulated data
    
    epsilon <- error_function(T, N, mean = 0, sd = 2)
    source('./data_generating/data_generating_99.R')
    
    #estimate naive-estimator
    
    temp_results[rep, c(1, 3)] <-
      within_est(X, Y, individual = FALSE, time = FALSE)
    
    #estimate within-estimator
    
    temp_results[rep, c(5, 7)] <-
      within_est(X, Y, individual = TRUE, time = TRUE)
    
    #estimate interactive-estimator
    
    temp_results[rep, c(9, 11)] <- Eup(
      Y ~ -1 + X[,,1] + X[,,2],
      factor.dim = 3
    )$slope.para
    
    #estimate interactive-estimator
    
    temp_results[rep, c(13, 15)] <- Eup(
      Y ~ -1 + X[,,1] + X[,,2],
      additive.effects = 'individual',
      factor.dim = 2
    )$slope.para
    
    setTxtProgressBar(progress, iter * n_rep + rep)
    
  }
  
  for (k in seq(2, 16, 2)) {
    temp_results[, k] <-
      sqrt(mean((temp_results[, k - 1] - mean(temp_results[, k - 1])) ^ 2))
  }
  
  results[iter + 1, 3:18] <- colMeans(temp_results, na.rm = TRUE)
  
  iter <- iter + 1
  
}

saveRDS(results, './output/table_99.rds')
write.csv(results, './output/table_99.csv')

table <- xtable(results, digits = 3, auto = TRUE)
print.xtable(table, include.rownames = FALSE)
