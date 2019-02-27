library(plm)

# Weighted data from Voigtlaender.
data_weighted <- readRDS('./data_application/original_data/data_weighted.rds')

# Fixed effects Bootstrap
#B <- 1000 used in paper!!
B <- 10
b_hat_plm <- array(data = NA, dim = c(7, 6, B))

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

  b_hat_plm[1, 1, b] <- weighted_boost_1$coefficients[1]  
  
  
  weighted_boost_2 <-
    plm(
      h ~ s_h_avg,
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )

  b_hat_plm[1, 2, b] <- weighted_boost_2$coefficients[1]
  
    
  weighted_boost_3 <-
    plm(
      reformulate(paste('s_h_avg', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat_plm[1, 3, b] <- weighted_boost_3$coefficients[1]
  
  
  weighted_boost_4 <-
    plm(
      reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat_plm[1:4, 4, b] <- weighted_boost_4$coefficients[1:4]
  
  
  weighted_boost_5 <-
    plm(
      reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat_plm[1:7, 5, b] <- weighted_boost_5$coefficients[1:7]  
  
  
  weighted_boost_6 <-
    plm(
      reformulate(paste('s2d_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )

  b_hat_plm[1:7, 6, b] <- weighted_boost_6$coefficients[1:7]
  
  
  setTxtProgressBar(progress, b)
    
} 

# Compute mean of betas.
b_hat_plm_mean <- apply(b_hat_plm, c(1,2), mean)

# Intialize container to store SEs results.
se_b_hat_plm <- matrix(data = 0, nrow = 7, ncol = 6)

# Compute standard errors form variance forumla.
for (j in 1:B) {
  se_b_hat_plm <- se_b_hat_plm + (b_hat_plm[,,j] - b_hat_plm_mean)^2
}
se_b_hat_plm <- sqrt(1 / (B - 1) * se_b_hat_plm)

# Save results.
saveRDS(se_b_hat_plm, "./data_application/tables/output/stand_errs_original_model.rds")
