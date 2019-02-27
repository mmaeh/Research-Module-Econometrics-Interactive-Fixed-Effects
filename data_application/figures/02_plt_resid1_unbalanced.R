data_weighted_w6 <-
  data_weighted[which(!is.na(data_weighted[, c('Outs_na')])), ]
weighted_5 <-
  plm(
    reformulate(
      paste(
        's_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff',
        time_dummies,
        sep = '+'
      ),
      'h'
    ),
    data = data_weighted_w6,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

png(
  filename = paste("./figures/", "w5_resid1", ".png", sep = ""),
  type = "cairo",
  units = "in",
  width = 10,
  height = 15,
  res = 300
)

data_weighted_w6[, 'resid'] <-
  sapply(seq(1:16633), function(x)
    weighted_5$residuals[[x]])
data_weighted_w6[, 'resid_stan'] <-
  sapply(data_weighted_w6['resid'], function(x)
    (x - mean(data_weighted_w6[, 'resid'])) / sd(data_weighted_w6[, 'resid']))
bwr.colors <-
  colorRampPalette(c("yellow", "green", "blue", "green", "yellow"))

print(
  wireframe(
    resid_stan ~ Sector + year,
    data = data_weighted_w6,
    drape = TRUE,
    aspect = c(0.6, 1),
    col.regions = bwr.colors(150),
    at = seq(-6, 6, length = 150),
    scales = list(arrows = FALSE),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = list(space = "right", height = 0.7),
    zlab = "Standardized \n Residuals",
    ylab = "Year"
  )
)

dev.off()
