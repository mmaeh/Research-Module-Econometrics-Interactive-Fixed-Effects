B <- 500

b_hat <- matrix(NA, nrow = B, ncol = 51)

for (b in 1:B) {

  # Eup bootstrap
  "index <- sample(1:358, 358, replace=TRUE)
  
  temp_df <- vars_weighted[,index,]
  
  weighted_interactive4 <- Eup(
    temp_df[, , 'h'] ~ temp_df[, , 's_h_avg'] + temp_df[, , 'equip_pw'] + temp_df[, , 'OCAMovK'] + temp_df[, , 'HT_diff'],
    additive.effects = 'individual',
    dim.criterion = 'IPC2'
    )
  
  b_hat[b, 1:4] <- weighted_interactive4$slope.para"
  
  # plm bootstrap
  sectors <- unique(data_weighted$Sector)
  index <- sample(sectors, 358, replace=TRUE)
  temp_df <- do.call(rbind, lapply(index, function(x) data_weighted[data_weighted['Sector'] == x,]))
  temp_df['index'] <- rep(1:358, each = 48)
  
  weighted_boost_4 <-
    plm(
      reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
      data = temp_df,
      effect = 'individual',
      model = 'within',
      index = c('index', 'year')
    )
  
  b_hat[b, 1:51] <- weighted_boost_4$coefficients
  
} 

b_hat_mean <- colMeans(b_hat)

se_b_hat <- lapply(b_hat_mean, function(x) sqrt(1 / (B - 1) * sum((b_hat[,which(b_hat_mean==x)] - x)^2)))

#index <- paste("c(", paste(sample(1:i, i, replace = TRUE), collapse = ","), ")", sep = "")
  
"
library(clusterSEs)

abc <- cluster.bs.plm(weighted_4, dat = data_weighted, cluster = 'group', boot.reps = 500, cluster.se = TRUE)"
