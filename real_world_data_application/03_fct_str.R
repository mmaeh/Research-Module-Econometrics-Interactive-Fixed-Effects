library(ggplot2)

data_weighted['two_digit_code'] <- sapply(data_weighted['Sector'], function(x) substr(x, start = 1, stop = 2))
data_weighted <- merge(data_weighted, sic_overreaching_sectors, by.x = "two_digit_code", by.y = "code")
data_weighted['code_1'] <- as.factor(data_weighted[,'code_1'])


#col_vector <- c('#F8766D', '#00BA38', '#619CFF', '#F8766D', '#00BA38', '#619CFF', '#F8766D', '#00BA38', '#619CFF', '#F8766D', '#00BA38', '#619CFF', 'black')
col_vector <- c('dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'dodgerblue2', 'darkorange2', 'grey', 'black')
line_vector <- c('solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed')
shape_vector <- c(15,15,15,16,16,16,15,15,15,16,16,16,32)
name_vector <- as.character(unique(data_weighted$name_1))


# Eup, Factor dim = 3, wo FE

weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

data_weighted['fac_str'] <- as.vector(weighted_interactive4$unob.fact.stru)

ggplot(data = data_weighted, aes(x = year, y = fac_str, group = Sector, linetype = code_1, color = code_1, shape = code_1)) + 
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_linetype_manual(values = line_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector) +
  ylab("Factor Structure") + 
  xlab("Year") + 
  labs(color = "Sector", shape = "Sector", linetype = "Sector") + 
  theme_minimal(base_size = 17)

ggsave("factor_str_dim3.png", dpi = 600, width = 20, height = 20)


# Kss, Factor dim = 3

weighted_interactive4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  factor.dim = 3
)

data_weighted['fac_str'] <- as.vector(weighted_interactive4$unob.fact.stru)

ggplot(data = data_weighted, aes(x = year, y = fac_str, group = Sector, linetype = code_1, color = code_1, shape = code_1)) + 
  geom_line(size = 1.1) +
  geom_point(size = 3) + 
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_linetype_manual(values = line_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector) +
  ylab("Factor Structure") + 
  xlab("Year") + 
  labs(color = "Sector", shape = "Sector", linetype = "Sector") + 
  theme_minimal(base_size = 17)

ggsave("factor_str_dim3_kss.png", dpi = 600, width = 20, height = 20)


# Kss, Factor dim = 2, sector FE

weighted_interactive4 <- KSS(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'individual',
  factor.dim = 2
)

data_weighted['fac_str'] <- as.vector(weighted_interactive4$unob.fact.stru)

ggplot(data = data_weighted, aes(x = year, y = fac_str, group = Sector, linetype = code_1, color = code_1, shape = code_1)) + 
  geom_line(size = 1.1) +
  geom_point(size = 3) + 
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_linetype_manual(values = line_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector) +
  ylab("Factor Structure") + 
  xlab("Year") + 
  labs(color = "Sector", shape = "Sector", linetype = "Sector") + 
  theme_minimal(base_size = 17)

ggsave("factor_str_dim2_kss.png", dpi = 600, width = 20, height = 20)


