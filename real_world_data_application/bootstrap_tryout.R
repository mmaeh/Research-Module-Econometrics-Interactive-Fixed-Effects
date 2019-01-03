# Fixed effects Bootstrap

B <- 1000
b_hat <- array(data = NA, dim = c(7, 6, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {
  
  # plm bootstrap
  sectors <- unique(data_weighted$Sector)
  index <- sample(sectors, 358, replace=TRUE)
  temp_df <- do.call(rbind, lapply(index, function(x) data_weighted[data_weighted['Sector'] == x,]))
  temp_df['index'] <- rep(1:358, each = 48)
  
  weighted_boost_1 <-
    lm(
      h ~ s_h_avg + const - 1, 
      data = temp_df
    )

  b_hat[1, 1, b] <- weighted_boost_1$coefficients[1]  
  
  
  weighted_boost_2 <-
    plm(
      h ~ s_h_avg,
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )

  b_hat[1, 2, b] <- weighted_boost_2$coefficients[1]
  
    
  weighted_boost_3 <-
    plm(
      reformulate(paste('s_h_avg', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat[1, 3, b] <- weighted_boost_3$coefficients[1]
  
  
  weighted_boost_4 <-
    plm(
      reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat[1:4, 4, b] <- weighted_boost_4$coefficients[1:4]
  
  
  weighted_boost_5 <-
    plm(
      reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat[1:7, 5, b] <- weighted_boost_5$coefficients[1:7]  
  
  
  weighted_boost_6 <-
    plm(
      reformulate(paste('s2d_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )

  b_hat[1:7, 6, b] <- weighted_boost_6$coefficients[1:7]
  
  
  setTxtProgressBar(progress, b)
    
} 

b_hat_mean <- apply(b_hat, c(1,2), mean)

se_b_hat <- matrix(data = 0, nrow = 7, ncol = 6)

for (j in 1:B) {
  se_b_hat <- se_b_hat + (b_hat[,,j] - b_hat_mean)^2
}

se_b_hat <- sqrt(1 / (B - 1) * se_b_hat)


# Interactive Bootstrap IPC1

B <- 1000
b_hat_ipc1 <- array(data = NA, dim = c(7, 4, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)

for (b in 1:B) {

  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  
  
  weighted_int_boost_3 <- Eup(
    temp_df[, , 'h'] ~ -1 + temp_df[, , 's_h_avg'],
    additive.effects = 'twoways',
    dim.criterion = 'IPC1'
  )

  b_hat_ipc1[1, 1, b] <- weighted_int_boost_3$slope.para
  
    
  weighted_int_boost_4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'IPC1'
  )
  
  b_hat_ipc1[1:4, 2, b] <- weighted_int_boost_4$slope.para

  
  weighted_int_boost_5 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'IPC1'
  )
  
  b_hat_ipc1[1:7, 3, b] <- weighted_int_boost_5$slope.para
  
  
  weighted_int_boost_6 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's2d_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'IPC1'
  )
  
  b_hat_ipc1[1:7, 4, b] <- weighted_int_boost_6$slope.para

  
  setTxtProgressBar(progress, b)
    
}

b_hat_ipc1_mean <- apply(b_hat_ipc1, c(1,2), mean)

se_b_hat_ipc1 <- matrix(data = 0, nrow = 7, ncol = 4)

for (j in 1:B) {
  se_b_hat_ipc1 <- se_b_hat_ipc1 + (b_hat_ipc1[,,j] - b_hat_ipc1_mean)^2
}

se_b_hat_ipc1 <- sqrt(1 / (B - 1) * se_b_hat_ipc1)

saveRDS(se_b_hat_ipc1, "./real_world_data_application/se_b_hat_ipc1.rds")

# Interactive Bootstrap PC1

B <- 1000
b_hat_pc1 <- array(data = NA, dim = c(7, 4, B))

progress <- txtProgressBar(min = 1, max = B, style = 3)


for (b in 1:B) {
  
  index <- sample(1:358, 358, replace=TRUE)
  temp_df <- vars_weighted[,index,]
  
  
  weighted_int_boost_3 <- Eup(
    temp_df[, , 'h'] ~ -1 + temp_df[, , 's_h_avg'],
    additive.effects = 'twoways',
    dim.criterion = 'PC1'
  )
  
  b_hat_pc1[1, 1, b] <- weighted_int_boost_3$slope.para
  
  
  weighted_int_boost_4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'PC1'
  )
  
  b_hat_pc1[1:4, 2, b] <- weighted_int_boost_4$slope.para
  
  
  weighted_int_boost_5 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'PC1'
  )
  
  b_hat_pc1[1:7, 3, b] <- weighted_int_boost_5$slope.para
  
  
  weighted_int_boost_6 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's2d_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'] + temp_df[,,'RD_int_lag'] + temp_df[,,'Outs_na'] + temp_df[,,'Outs_diff'],
    additive.effects = 'twoways',
    dim.criterion = 'PC1'
  )
  
  b_hat_pc1[1:7, 4, b] <- weighted_int_boost_6$slope.para
  
  
  setTxtProgressBar(progress, b)
  
}

b_hat_pc1_mean <- apply(b_hat_pc1, c(1,2), mean)

se_b_hat_pc1 <- matrix(data = 0, nrow = 7, ncol = 4)

for (j in 1:B) {
  se_b_hat_pc1 <- se_b_hat_pc1 + (b_hat_pc1[,,j] - b_hat_pc1_mean)^2
}

se_b_hat_pc1 <- sqrt(1 / (B - 1) * se_b_hat_pc1)







library(clusterSEs)

abc <- cluster.bs.plm(weighted_4, dat = data_weighted, cluster = 'group', boot.reps = 100, cluster.se = TRUE)
