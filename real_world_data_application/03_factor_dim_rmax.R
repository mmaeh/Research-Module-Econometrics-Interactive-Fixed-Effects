library(xtable)

results_eup <- as.data.frame(matrix(NA,13,4))
results_eup[,1] <- c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR')

weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff']
)

for (j in c(5,10,15)) {
  opt_obj <- OptDim(weighted_interactive4, d.max = j, criteria = c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR'))
  results_eup[,j/5+1] <- t(opt_obj$summary)
}

table <- xtable(results_eup, auto = TRUE)
print.xtable(table, include.rownames=FALSE)



results_kss <- as.data.frame(matrix(NA,13,4))
results_kss[,1] <- c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR')

weighted_interactive4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff']
)

for (j in c(5,10,15)) {
  opt_obj <- OptDim(weighted_interactive4, d.max = j, criteria = c('PC1', 'PC2', 'PC3', 'BIC3', 'IC1', 'IC2', 'IC3', 'IPC1', 'IPC2', 'IPC3', 'ED', 'ER', 'GR'))
  results_kss[,j/5+1] <- t(opt_obj$summary)
}

table <- xtable(results_kss, auto = TRUE)
print.xtable(table, include.rownames=FALSE)
