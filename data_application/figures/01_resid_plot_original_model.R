library(lattice)
library(plm)

# Read in data.
data_weighted <- readRDS('./data_application/original_data/data_weighted.rds')

# Time dummies.
time_dummies <- "dummy_t1"

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
}

##################VOIGTLAENDER###################################
##RESIDUALS COLUMN 4#############################################
#################################################################

weighted_4 <-
  plm(
    reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

png(
  filename = "./data_application/figures/output/01_resid_plot_origal_model.png",
  type = "cairo",
  units = "in",
  width = 10,
  height = 15,
  res = 300
)

data_weighted[, 'resid'] <-
  sapply(seq(1:17184), function(x)
    weighted_4$residuals[[x]])
data_weighted[, 'resid_stan'] <-
  sapply(data_weighted['resid'], function(x)
    (x - mean(data_weighted[, 'resid'])) / sd(data_weighted[, 'resid']))
bwr.colors <-
  colorRampPalette(c("yellow", "green", "blue", "green", "yellow"))

print(
  wireframe(
    resid_stan ~ Sector + year,
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
  )
)

dev.off()
