library(FactoMineR)
library(gridExtra)
library(ggplot2)

# Eup case

plot_list_eup <- list()

for (i in 0:8) {

  weighted_interactive4 <- Eup(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'individual',
    factor.dim = i
  )
  
  resid_w4_inter <- weighted_interactive4$residuals
  
  nrow  <- nrow(resid_w4_inter)
  ncol  <- ncol(resid_w4_inter)
  if (nrow > ncol) {
    resid_Q <- t(resid_w4_inter) %*% resid_w4_inter
  } else {
    resid_Q <- resid_w4_inter %*% t(resid_w4_inter)
  }
  
  resid_pca <- PCA(resid_Q, graph = F)
  screenplot <- as.data.frame(matrix(NA, 15, 2))
  colnames(screenplot) <- c("n_factors", "var_perc")
  
  screenplot$n_factors <- 1:15
  screenplot$var_perc <- round(resid_pca$eig[, 2][1:15], 2)
  
  plot_list_eup[[i+1]] <- ggplot(data = screenplot, aes(x = n_factors, y = var_perc, fill = )) +
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
  filename = "./figures/screeplot_w4_eup.png",
  type = "cairo",
  units = "in",
  width = 17,
  height = 10,
  res = 300
)

do.call(grid.arrange, plot_list_eup)

dev.off()

# KSS case


plot_list_kss <- list()

for (i in 0:8) {
  
  weighted_interactive4 <- KSS(
    vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'individual',
    factor.dim = i
  )
  
  resid_w4_inter <- weighted_interactive4$residuals
  
  nrow  <- nrow(resid_w4_inter)
  ncol  <- ncol(resid_w4_inter)
  if (nrow > ncol) {
    resid_Q <- t(resid_w4_inter) %*% resid_w4_inter
  } else {
    resid_Q <- resid_w4_inter %*% t(resid_w4_inter)
  }
  
  resid_pca <- PCA(resid_Q, graph = F)
  screenplot <- as.data.frame(matrix(NA, 15, 2))
  colnames(screenplot) <- c("n_factors", "var_perc")
  
  screenplot$n_factors <- 1:15
  screenplot$var_perc <- round(resid_pca$eig[, 2][1:15], 2)
  
  plot_list_kss[[i+1]] <- ggplot(data = screenplot, aes(x = n_factors, y = var_perc, fill = )) +
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
  filename = "./figures/screeplot_w4_kss.png",
  type = "cairo",
  units = "in",
  width = 17,
  height = 10,
  res = 300
)

do.call(grid.arrange, plot_list_kss)

dev.off()
