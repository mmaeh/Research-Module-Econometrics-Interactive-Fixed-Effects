plt_resid1_inter <- function(model, name) {
  png(
    filename = paste("./figures/", name, ".png", sep = ""),
    type = "cairo",
    units = "in",
    width = 10,
    height = 15,
    res = 300
  )
  
  data_weighted[, 'resid_inter'] <- as.vector(model$residuals)
  data_weighted[, 'resid_inter_stan'] <-
    sapply(data_weighted['resid_inter'], function(x)
      (x - mean(data_weighted[, 'resid_inter'])) / sd(data_weighted[, 'resid_inter']))
  bwr.colors <-
    colorRampPalette(c("yellow", "green", "blue", "green", "yellow"))
  
  print(wireframe(
    resid_inter_stan ~ Sector + year,
    data = data_weighted,
    drape = TRUE,
    aspect = c(0.6, 1),
    col.regions = bwr.colors(150),
    at = seq(-6, 6, length = 150),
    scales = list(arrows = FALSE),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = list(space = "right", height = 0.7),
    zlab = "Standardized \n Residuals",
    ylab = "Year"
  ))
  
  dev.off()
  
}
