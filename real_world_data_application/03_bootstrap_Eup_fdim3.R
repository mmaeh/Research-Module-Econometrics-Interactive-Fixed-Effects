# Interactive Bootstrap Eup, fdim = 3, FE = none

B <- 1000
b_hat_eup_30 <- array(data = NA, dim = c(7, 4, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {

  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  
  
  weighted_eup_boost_3 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'],
    additive.effects = 'none',
    factor.dim = 3
  )

  b_hat_eup_30[1, 1, b] <- weighted_eup_boost_3$slope.para
  
    
  weighted_eup_boost_4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat_eup_30[1:4, 2, b] <- weighted_eup_boost_4$slope.para

  
  weighted_eup_boost_5 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat_eup_30[1:7, 3, b] <- weighted_eup_boost_5$slope.para
  
  
  weighted_eup_boost_6 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's2d_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'none',
    factor.dim = 3
  )
  
  b_hat_eup_30[1:7, 4, b] <- weighted_eup_boost_6$slope.para

  
  setTxtProgressBar(progress, b)
    
}

b_hat_eup_30_mean <- apply(b_hat_eup_30, c(1,2), mean)

se_b_hat_eup_30 <- matrix(data = 0, nrow = 7, ncol = 4)

for (j in 1:B) {
  se_b_hat_eup_30 <- se_b_hat_eup_30 + (b_hat_eup_30[,,j] - b_hat_eup_30_mean)^2
}

se_b_hat_eup_30 <- sqrt(1 / (B - 1) * se_b_hat_eup_30)

saveRDS(b_hat_eup_30, "./real_world_data_application/b_hat_eup_30.rds")
saveRDS(se_b_hat_eup_30, "./real_world_data_application/se_b_hat_eup_30.rds")



# # Interactive Bootstrap Eup, fdim = 2, FE = sector
# 
# B <- 1000
# b_hat_eup_21 <- array(data = NA, dim = c(7, 4, B))
# 
# progress <- txtProgressBar(min = 1, max = B, style = 3)
# 
# 
# for (b in 1:B) {
#   
#   index <- sample(1:358, 358, replace=TRUE)
#   temp_df <- vars_weighted[,index,]
#   
#   
#   weighted_eup_boost_3 <- Eup(
#     temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'],
#     additive.effects = 'individual',
#     factor.dim = 2
#   )
#   
#   b_hat_eup_21[1, 1, b] <- weighted_eup_boost_3$slope.para
#   
#   
#   weighted_eup_boost_4 <- Eup(
#     temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
#     additive.effects = 'individual',
#     factor.dim = 2
#   )
#   
#   b_hat_eup_21[1:4, 2, b] <- weighted_eup_boost_4$slope.para
#   
#   
#   weighted_eup_boost_5 <- Eup(
#     temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
#     additive.effects = 'individual',
#     factor.dim = 2
#   )
#   
#   b_hat_eup_21[1:7, 3, b] <- weighted_eup_boost_5$slope.para
#   
#   
#   weighted_eup_boost_6 <- Eup(
#     temp_df[, , 'h'] ~ temp_df[, , 's2d_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
#     additive.effects = 'individual',
#     factor.dim = 2
#   )
#   
#   b_hat_eup_21[1:7, 4, b] <- weighted_eup_boost_6$slope.para
#   
#   
#   setTxtProgressBar(progress, b)
#   
# }
# 
# b_hat_eup_21_mean <- apply(b_hat_eup_21, c(1,2), mean)
# 
# se_b_hat_eup_21 <- matrix(data = 0, nrow = 7, ncol = 4)
# 
# for (j in 1:B) {
#   se_b_hat_eup_21 <- se_b_hat_eup_21 + (b_hat_eup_21[,,j] - b_hat_eup_21_mean)^2
# }
# 
# se_b_hat_eup_21 <- sqrt(1 / (B - 1) * se_b_hat_pc1)
# 
# saveRDS(b_hat_eup_21, "./real_world_data_application/b_hat_eup_21.rds")
# saveRDS(se_b_hat_eup_21, "./real_world_data_application/se_b_hat_eup_21.rds")
