library(lattice)
library(phtt)

# Read in data.
data_weighted <- readRDS('./data_application/original_data/data_weighted.rds')
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')


##################BAI ESTIMATOR##################################
##Factor dimnesion == 3, no sector fixed-effects#################
#################################################################

ife_clmn4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

# Set .up png save.
png(
  filename = "./data_application/figures/output/A1_resid_plot_fdim3_kneip.png",
  type = "cairo",
  units = "in",
  width = 10,
  height = 15,
  res = 300
)

# Get standardized residuals.
data_weighted[, 'resid_inter'] <- as.vector(ife_clmn4$residuals)
data_weighted[, 'resid_inter_stan'] <-
  sapply(data_weighted['resid_inter'], function(x)
    (x - mean(data_weighted[, 'resid_inter'])) / sd(data_weighted[, 'resid_inter']))
bwr.colors <-
  colorRampPalette(c("yellow", "green", "blue", "green", "yellow"))

# Make residual plot.
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

