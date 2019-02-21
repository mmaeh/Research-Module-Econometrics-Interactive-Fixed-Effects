library(phtt)

results <- as.data.frame(matrix(NA, 7, 4))

# column 1-3
weighted_interactive3 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'],
  additive.effects = 'none',
  factor.dim = 3
)

results[1,1] <- weighted_interactive3$slope.para


# column 4
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

wi4 <- summary(weighted_interactive4)
results[1:4,2] <- weighted_interactive4$slope.para


# column 5
weighted_interactive5 <- Eup(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

results[1:7,3] <- weighted_interactive5$slope.para


# column 6
weighted_interactive6 <- Eup(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's2d_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

results[1:7,4] <- weighted_interactive6$slope.para

saveRDS(results, "coeff_ife_fdim3.rds")