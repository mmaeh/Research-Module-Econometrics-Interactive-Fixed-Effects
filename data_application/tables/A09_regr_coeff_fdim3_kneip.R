library(phtt)

# Weighted data from Voigtlaender.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')
vars_weighted_bal <- readRDS('./data_application/original_data/vars_weighted_bal.rds')

# dataframe to store results
results <- as.data.frame(matrix(NA, 7, 4))

# column 1 - 3
weighted_interactive1 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'],
  factor.dim = 3
)

results[1,1] <- weighted_interactive1$slope.para


# column 4
weighted_interactive4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  factor.dim = 3
)

results[1:4,2] <- weighted_interactive4$slope.para


# column 5
weighted_interactive5 <- KSS(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  factor.dim = 3
)

results[1:7,3] <- weighted_interactive5$slope.para


# column 6
weighted_interactive6 <- KSS(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's2d_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  factor.dim = 3
)

results[1:7,4] <- weighted_interactive6$slope.para


#Save data.
saveRDS(results, "./data_application/tables/output/A09_regression_coeff.rds")