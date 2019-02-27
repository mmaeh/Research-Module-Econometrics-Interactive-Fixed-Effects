library(FactoMineR)
library(gridExtra)
library(ggplot2)

# Read in data.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')

##################BAI ESTIMATOR##################################
##Factor dimnesion == 3, sector fixed-effects####################
#################################################################

# List to store individual plots.
plot_list <- list()

for (i in 0:8) {

  ife_clmn4 <- Eup(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'none',
    factor.dim = i
  )
  
  ife_clmn4 <- ife_clmn4$residuals
  
  nrow  <- nrow(ife_clmn4)
  ncol  <- ncol(ife_clmn4)
  if (nrow > ncol) {
    resid_Q <- t(ife_clmn4) %*% ife_clmn4
  } else {
    resid_Q <- ife_clmn4 %*% t(ife_clmn4)
  }
  
  resid_pca <- PCA(resid_Q, graph = F)
  screenplot <- as.data.frame(matrix(NA, 15, 2))
  colnames(screenplot) <- c("n_factors", "var_perc")
  
  screenplot$n_factors <- 1:15
  screenplot$var_perc <- round(resid_pca$eig[, 2][1:15], 2)
  
  plot_list[[i+1]] <- ggplot(data = screenplot, aes(x = n_factors, y = var_perc, fill = )) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = var_perc), vjust = -1) +
    ggtitle(paste('R=', i, sep = '')) +
    xlab('Ordered eigenvalues') + 
    scale_x_continuous(breaks=1:15, labels=c(as.character(1:15))) + 
    ylab('Explained variance in %') + 
    ylim(0, screenplot$var_perc[1] + 5) + 
    theme_minimal()
}

png(
  filename = "./data_application/figures/output/02_screeplot_fdim3_bai.png",
  type = "cairo",
  units = "in",
  width = 17,
  height = 10,
  res = 300
)

do.call(grid.arrange, plot_list)

dev.off()
