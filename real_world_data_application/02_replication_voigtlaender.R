# regressions from voiglaender Table 3

# column 1 - here the estimate is off???
weighted_1 <-
  lm(
    h ~ s_h_avg + const - 1, 
    data = data_weighted
  )

coeftest(weighted_1, cluster.vcov(weighted_1, data_weighted$Sector)) # maybe change the type here

# column 2
weighted_2 <-
  plm(
    h ~ s_h_avg,
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

coeftest(weighted_2, vcov = vcovHC(weighted_2, type = "HC1", cluster = "group"))

# column 3

weighted_3 <-
  plm(
    reformulate(paste('s_h_avg', time_dummies, sep = '+'), 'h'),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

coeftest(weighted_3, vcov = vcovHC(weighted_3, type = "HC1", cluster = "group"))

# column 4

weighted_4 <-
  plm(
    reformulate(paste('s_h_avg + equip_pw + OCAMovK + HT_diff', time_dummies, sep = '+'), 'h'),
    data = data_weighted,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )

testmodel1 <- plm(h ~ s_h_avg + equip_pw + OCAMovK + HT_diff, data = data_weighted, model = "within", effect = "individual")
testmodel2 <- plm(h ~ s_h_avg + equip_pw + OCAMovK + HT_diff, data = data_weighted, model = "within", effect = "twoways")
phtest(testmodel, testmodel2)

coeftest(weighted_4, vcov = vcovHC(weighted_4, type = "HC1", cluster = "group"))

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

coeftest(weighted_5, vcov = vcovHC(weighted_5, type = "HC1", cluster = "group"))

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
