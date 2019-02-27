library(ggplot2)

# Read in data.
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')

##################KNEIP ESTIMATOR################################
##Factor dimnesion == 2, sector fixed-effects#################
################################################################

ife_clmn4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'individual',
  factor.dim = 2
)

df_unob_fctrs <- as.data.frame(matrix(NA,48*2,3))
colnames(df_unob_fctrs) <- c('unob_fctrs', 'year', 'n_factor')

df_unob_fctrs['unob_fctrs'] <- as.vector(ife_clmn4$unob.factors)
df_unob_fctrs['year'] <- rep(1:48, times = 2)
df_unob_fctrs['n_factor'] <- as.factor(rep(1:2, each = 48))

# Plot unobserved factors.
ggplot(data = df_unob_fctrs, aes(x = year, y = unob_fctrs, group = n_factor, linetype = n_factor, color = n_factor)) + 
  geom_line(size = 1.5) +
  scale_colour_manual(values = c('darkorange2', 'grey60')) + 
  scale_linetype_manual(values = c("twodash", "dashed")) +
  ylab("Unobserved Factors") + 
  xlab("Year") + 
  labs(linetype = "Factors", color = "Factors") +
  theme_minimal(base_size = 17)

ggsave("./data_application/figures/output/A3_unobs_fct_fdim2_kneip.png", dpi = 600, width = 10, height = 20)
