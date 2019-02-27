library(plm)
library(lmtest)

# Weighted data from Voigtlaender.
data_weighted <- readRDS('./data_application/original_data/data_weighted.rds')

# Time dummies.
time_dummies <- "dummy_t1"

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
}

# column 1 
weighted_1 <-
  lm(
    h ~ s_h_avg + const - 1, 
    data = data_weighted
  )

cov_weighted_1 <- coeftest(weighted_1, vcov = vcovHC(weighted_1, type = "HC1", cluster = "group")) 

# column 2
weighted_2 <-
  plm(
    h ~ s_h_avg,
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

cov_weighted_2 <- coeftest(weighted_2, vcov = vcovHC(weighted_2, type = "HC1", cluster = "group"))

# column 3

weighted_3 <-
  plm(
    reformulate(paste('s_h_avg', time_dummies, sep = '+'), 'h'),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

cov_weighted_3 <- coeftest(weighted_3, vcov = vcovHC(weighted_3, type = "HC1", cluster = "group"))

# column 4

weighted_4 <-
  plm(
    reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

cov_weighted_4 <- coeftest(weighted_4, vcov = vcovHC(weighted_4, type = "HC1", cluster = "group"))

# column 5

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
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

cov_weighted_5 <- coeftest(weighted_5, vcov = vcovHC(weighted_5, type = "HC1", cluster = "group"))

# column 6

weighted_6 <-
  plm(
    reformulate(
      paste(
        's2d_h_avg + equip_pw + OCAMovK + HT_diff + RD_int_lag + Outs_na + Outs_diff',
        time_dummies,
        sep = '+'
      ),
      'h'
    ),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

cov_weighted_6 <- coeftest(weighted_6, vcov = vcovHC(weighted_6, type = "HC1", cluster = "group"))
