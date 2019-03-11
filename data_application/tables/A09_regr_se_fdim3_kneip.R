library(phtt)

# Weighted data from Voigtlaender.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')
vars_weighted_bal <- readRDS('./data_application/original_data/vars_weighted_bal.rds')

# Interactive Bootstrap, Kneip estimator, fdim = 3, FE = none
# Array to store bootstrap results.
B <- 1000
b_hat <- array(data = NA, dim = c(7, 4, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {

  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  
  # Column 1-3
  weighted_eup_boost_3 <- KSS(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'],
    additive.effects = 'none',
    factor.dim = 3
  )

  b_hat[1, 1, b] <- weighted_eup_boost_3$slope.para
  
  # Column 4
  weighted_eup_boost_4 <- KSS(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat[1:4, 2, b] <- weighted_eup_boost_4$slope.para

  
  ################
  #Balanced sample
  index_bal <- sample(1:329, 329, replace=TRUE)
  temp_bal_df <- vars_weighted_bal[,index_bal,]
  
  # Column 5
  weighted_eup_boost_5 <- KSS(
    temp_bal_df[, , 'h'] ~ temp_bal_df[, , 's_h_avg'] + temp_bal_df[, , 'equip_pw'] + temp_bal_df[, , 'OCAMovK'] + temp_bal_df[, , 'HT_diff'] + temp_bal_df[,,'RD_int_lag'] + temp_bal_df[,,'Outs_na'] + temp_bal_df[,,'Outs_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat[1:7, 3, b] <- weighted_eup_boost_5$slope.para
  
  # Column 6
  weighted_eup_boost_6 <- KSS(
    temp_bal_df[, , 'h'] ~ temp_bal_df[, , 's2d_h_avg'] + temp_bal_df[, , 'equip_pw'] + temp_bal_df[, , 'OCAMovK'] + temp_bal_df[, , 'HT_diff'] + temp_bal_df[,,'RD_int_lag'] + temp_bal_df[,,'Outs_na'] + temp_bal_df[,,'Outs_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat[1:7, 4, b] <- weighted_eup_boost_6$slope.para

  
  setTxtProgressBar(progress, b)
    
}

# Compute regression coefficient mean.
b_hat_mean <- apply(b_hat, c(1,2), mean)

# Container to store standard errors.
se_b_hat <- matrix(data = 0, nrow = 7, ncol = 4)

# Compute standard errors according to variance formula.
for (j in 1:B) {
  se_b_hat <- se_b_hat + (b_hat[,,j] - b_hat_mean)^2
}
se_b_hat <- sqrt(1 / (B - 1) * se_b_hat)

# Save data.
saveRDS(se_b_hat, "./data_application/tables/output/A09_standard_errors.rds")
