library(phtt)

# Load data.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')

# Container to store results
results <- as.data.frame(matrix(NA,13,4))
# Column names.
results[,1] <- c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR')

# IFE model corresponding to column 4 in Table 3 of Voigtlaender.
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff']
)

# Estimated factor fimension for various criteria and Rmax.
for (j in c(5,10,15)) {
  opt_obj <- OptDim(weighted_interactive4, d.max = j, criteria = c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR'))
  results[,j/5+1] <- t(opt_obj$summary)
}

# Save results.
saveRDS(results, "./data_application/tables/output/06_fdim_criteria_bai.rds")
