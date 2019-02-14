# This 
library(phtt)
library(xtable)

# Eup results
results_w4_Eup <- matrix(NA, 8, 9)

for (i in 0:8) {
  weighted_interactive4 <- Eup(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    d.max = 8,
    factor.dim = i
  )
  
  print(summary(weighted_interactive4))
  
  results_w4_Eup[c(1,3,5,7),i+1] <- summary(weighted_interactive4)$coefficients[2:5,1]
  results_w4_Eup[c(2,4,6,8),i+1] <- summary(weighted_interactive4)$coefficients[2:5,2]
}

table <- xtable(results_w4_Eup, digit = 3, auto = TRUE)
print.xtable(table, include.rownames = F)

# KSS results
results_w4_KSS <- matrix(NA, 8, 9)

for (i in 0:8) {
  weighted_interactive4 <- KSS(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    d.max = 8,
    factor.dim = i
  )
  
  print(summary(weighted_interactive4))
  
  results_w4_KSS[c(1,3,5,7),i+1] <- summary(weighted_interactive4)$coefficients[2:5,1]
  results_w4_KSS[c(2,4,6,8),i+1] <- summary(weighted_interactive4)$coefficients[2:5,2]
}

table <- xtable(results_w4_KSS, digit = 3, auto = TRUE)
print.xtable(table, include.rownames = F)
