library(phtt)
library(xtable)

# Eup results

results_w4_Eup <- matrix(NA, 4, 9)

for (i in 0:8) {
  weighted_interactive4 <- Eup(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    d.max = 8,
    factor.dim = i
  )
  
  results_w4_Eup[,i+1] <- weighted_interactive4$slope.para
}

table <- xtable(results_w4_Eup, digit = 3, auto = TRUE)
print.xtable(table, include.rownames = F)

# KSS results

results_w4_KSS <- matrix(NA, 4, 9)

for (i in 0:8) {
  weighted_interactive6 <- KSS(
    vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    d.max = 8,
    factor.dim = i
  )
  
  results_w4_KSS[,i+1] <- weighted_interactive6$slope.para
}

table <- xtable(results_w4_KSS, digit = 3, auto = TRUE)
print.xtable(table, include.rownames = F)


# SEs
# Eup, fdim = 3, FE = none, specification = 4

B <- 1000
b_hat_eup_30 <- array(data = NA, dim = c(4, 9, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {
  
  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  

  weighted_eup_boost_0 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 0
  )
  
  b_hat_eup_30[, 1, b] <- weighted_eup_boost_0$slope.para
  
  
  weighted_eup_boost_1 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 1
  )
  
  b_hat_eup_30[, 2, b] <- weighted_eup_boost_1$slope.para
  
  
  weighted_eup_boost_2 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 2
  )
  
  b_hat_eup_30[, 3, b] <- weighted_eup_boost_2$slope.para
  
  
  weighted_eup_boost_3 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat_eup_30[, 4, b] <- weighted_eup_boost_3$slope.para
  
    
  weighted_eup_boost_4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 4
  )
  
  b_hat_eup_30[, 5, b] <- weighted_eup_boost_4$slope.para
  
  
  weighted_eup_boost_5 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 5
  )
  
  b_hat_eup_30[, 6, b] <- weighted_eup_boost_5$slope.para
  
  
  weighted_eup_boost_6 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 6
  )
  
  b_hat_eup_30[, 7, b] <- weighted_eup_boost_6$slope.para
  
  
  weighted_eup_boost_7 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 7
  )
  
  b_hat_eup_30[, 8, b] <- weighted_eup_boost_7$slope.para
  
  
  weighted_eup_boost_8 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 8
  )
  
  b_hat_eup_30[, 9, b] <- weighted_eup_boost_8$slope.para
  
  setTxtProgressBar(progress, b)
  
}

b_hat_eup_30_mean <- apply(b_hat_eup_30, c(1,2), mean)

se_b_hat_eup_30 <- matrix(data = 0, nrow = 9, ncol = 4)

for (j in 1:B) {
  se_b_hat_eup_30 <- se_b_hat_eup_30 + (b_hat_eup_30[,,j] - b_hat_eup_30_mean)^2
}

se_b_hat_eup_30 <- sqrt(1 / (B - 1) * se_b_hat_eup_30)

saveRDS(b_hat_eup_30, "./real_world_data_application/b_hat_eup_multipleR.rds")
saveRDS(se_b_hat_eup_30, "./real_world_data_application/se_b_hat_eup_multipleR.rds")


# SEs
# KSS, fdim = 3, FE = none, specification = 4

B <- 1000
b_hat_kss_30 <- array(data = NA, dim = c(4, 9, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {
  
  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  
  
  weighted_kss_boost_0 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 0
  )
  
  b_hat_kss_30[, 1, b] <- weighted_kss_boost_0$slope.para
  
  
  weighted_kss_boost_1 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 1
  )
  
  b_hat_kss_30[, 2, b] <- weighted_kss_boost_1$slope.para
  
  
  weighted_kss_boost_2 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 2
  )
  
  b_hat_kss_30[, 3, b] <- weighted_kss_boost_2$slope.para
  
  
  weighted_kss_boost_3 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat_kss_30[, 4, b] <- weighted_kss_boost_3$slope.para
  
  
  weighted_kss_boost_4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 4
  )
  
  b_hat_kss_30[, 5, b] <- weighted_kss_boost_4$slope.para
  
  
  weighted_kss_boost_5 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 5
  )
  
  b_hat_kss_30[, 6, b] <- weighted_kss_boost_5$slope.para
  
  
  weighted_kss_boost_6 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 6
  )
  
  b_hat_kss_30[, 7, b] <- weighted_kss_boost_6$slope.para
  
  
  weighted_kss_boost_7 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 7
  )
  
  b_hat_kss_30[, 8, b] <- weighted_kss_boost_7$slope.para
  
  
  weighted_kss_boost_8 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 8
  )
  
  b_hat_kss_30[, 9, b] <- weighted_kss_boost_8$slope.para
  
  setTxtProgressBar(progress, b)
  
}

b_hat_kss_30_mean <- apply(b_hat_kss_30, c(1,2), mean)

se_b_hat_kss_30 <- matrix(data = 0, nrow = 9, ncol = 4)

for (j in 1:B) {
  se_b_hat_kss_30 <- se_b_hat_kss_30 + (b_hat_kss_30[,,j] - b_hat_kss_30_mean)^2
}

se_b_hat_kss_30 <- sqrt(1 / (B - 1) * se_b_hat_kss_30)

saveRDS(b_hat_kss_30, "./real_world_data_application/b_hat_kss_multipleR.rds")
saveRDS(se_b_hat_kss_30, "./real_world_data_application/se_b_hat_kss_multipleR.rds")
