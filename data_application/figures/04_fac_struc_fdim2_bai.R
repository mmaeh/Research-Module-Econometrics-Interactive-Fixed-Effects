library(ggplot2)
library(xlsx)

# Read in data.
data_weighted <- readRDS('./data_application/original_data/data_weighted.rds')
vars_weighted <- readRDS('./data_application/original_data/vars_weighted.rds')

# Merge SIC two digit code.
sic_overreaching_sectors <- read.xlsx('./data_application/original_data/sic_overreaching_sectors.xlsx', sheetIndex = 1)

data_weighted['two_digit_code'] <- sapply(data_weighted['Sector'], function(x) substr(x, start = 1, stop = 2))
data_weighted <- merge(data_weighted, sic_overreaching_sectors, by.x = "two_digit_code", by.y = "code")
data_weighted['code_1'] <- as.factor(data_weighted[,'code_1'])

# Optics in the figure.
col_vector <- c('dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'black')
line_vector <- c('solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed')
shape_vector <- c(15,15,15,16,16,16,15,15,15,16,16,16,32)
name_vector <- as.character(unique(data_weighted$name_1))


##################BAI ESTIMATOR##################################
##Factor dimnesion == 2, sector fixed-effects####################
#################################################################

# Interactive model corresponding to column 4 of Voigtlaender model.
ife_clmn4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'individual',
  factor.dim = 2
)

# Get factor structure to dataframe.
data_weighted['fctr_str_dim2_bai'] <- as.vector(ife_clmn4$unob.fact.stru)

# Plot factor structure.
ggplot(data = data_weighted, aes(x = year, y = fctr_str_dim2_bai, group = Sector, linetype = code_1, color = code_1, shape = code_1)) + 
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_linetype_manual(values = line_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector) +
  ylab("Factor Structure") + 
  xlab("Year") + 
  labs(color = "Sector", shape = "Sector", linetype = "Sector") + 
  theme_minimal(base_size = 17)

# Save figure.
ggsave("./data_application/figures/output/04_fctstr_fdim2_bai.png", dpi = 600, width = 20, height = 20)

