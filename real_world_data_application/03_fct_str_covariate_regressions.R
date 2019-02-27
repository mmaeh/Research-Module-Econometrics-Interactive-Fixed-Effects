library(phtt)
library(plm)

vars_weighted <- readRDS('./real_world_data_application/vars_weighted.rds')
data_weighted <- readRDS('./real_world_data_application/data_weighted.rds')
data_weighted_bal <- readRDS('./real_world_data_application/data_weighted_bal.rds')


# column 1
weighted_interactive1 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'],
  additive.effects = 'none',
  factor.dim = 3
)

data_weighted['factor_str_wi1'] <- as.vector(weighted_interactive1$unob.fact.stru)

w1_s_h_avg <- plm('s_h_avg ~ factor_str_wi1', data = data_weighted)

# column 4
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

data_weighted['factor_str_wi4'] <- as.vector(weighted_interactive4$unob.fact.stru)

w4_s_h_avg <- plm(s_h_avg ~ factor_str_wi4, data = data_weighted, model = 'within', effect = 'individual')
summary(w4_s_h_avg)

w4_equip_pw <- plm(equip_pw ~ factor_str_wi4, data = data_weighted, model = 'within', effect = 'individual')
summary(w4_equip_pw)

w4_OCAMovK <- plm(OCAMovK ~ factor_str_wi4, data = data_weighted, model = 'within', effect = 'individual')
summary(w4_OCAMovK)

w4_HT_diff <- plm(HT_diff ~ factor_str_wi4, data = data_weighted, model = 'within', effect = 'individual')
summary(w4_HT_diff)

# column 5
weighted_interactive5 <- Eup(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

data_weighted_bal['factor_str_wi5'] <- as.vector(weighted_interactive5$unob.fact.stru)

w5_s_h_avg <- plm(s_h_avg ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_s_h_avg)

w5_equip_pw <- plm(equip_pw ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_equip_pw)

w5_OCAMovK <- plm(OCAMovK ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_OCAMovK)

w5_HT_diff <- plm(HT_diff ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_HT_diff)

w5_RD_int_lag <- plm(RD_int_lag ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_RD_int_lag)

w5_Outs_na <- plm(Outs_na ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_Outs_na)

w5_Outs_diff <- plm(Outs_diff ~ factor_str_wi5, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w5_Outs_diff)


# column 6
weighted_interactive6 <- Eup(
  vars_weighted_bal[, , 'h'] ~ vars_weighted_bal[, , 's2d_h_avg'] + vars_weighted_bal[, , 'equip_pw'] + vars_weighted_bal[, , 'OCAMovK'] + vars_weighted_bal[, , 'HT_diff'] + vars_weighted_bal[,,'RD_int_lag'] + vars_weighted_bal[,,'Outs_na'] + vars_weighted_bal[,,'Outs_diff'],
  additive.effects = 'none',
  factor.dim = 3
)

data_weighted_bal['factor_str_wi6'] <- as.vector(weighted_interactive6$unob.fact.stru)

w6_s_h_avg <- plm(s_h_avg ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_s_h_avg)

w6_equip_pw <- plm(equip_pw ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_equip_pw)

w6_OCAMovK <- plm(OCAMovK ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_OCAMovK)

w6_HT_diff <- plm(HT_diff ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_HT_diff)

w6_RD_int_lag <- plm(RD_int_lag ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_RD_int_lag)

w6_Outs_na <- plm(Outs_na ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_Outs_na)

w6_Outs_diff <- plm(Outs_diff ~ factor_str_wi6, data = data_weighted_bal, model = 'within', effect = 'individual')
summary(w6_Outs_diff)
