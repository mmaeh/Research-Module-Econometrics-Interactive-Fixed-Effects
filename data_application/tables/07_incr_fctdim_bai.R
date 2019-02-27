library(phtt)

# Weighted data from Voigtlaender.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')

# Container to store results.
results <- matrix(NA, 8, 9)

for (i in 0:8) {
  # Column 4 specification.
  regression <- Eup(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    d.max = 8,
    factor.dim = i
  )
  
  results[c(1,3,5,7),i+1] <- summary(regression)$coefficients[2:5,1]
  results[c(2,4,6,8),i+1] <- summary(regression)$coefficients[2:5,2]
}

#Save data.
saveRDS(results, "./data_application/tables/output/07_incr_fctdim_bai.rds")
