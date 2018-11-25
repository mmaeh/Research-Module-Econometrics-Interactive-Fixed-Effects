library(foreign)
library(lfe)
library(lmtest)
library(multiwayvcov)
library(phtt)
library(plm)
library(stargazer)
library(xlsx)

# read in data from voigtlaender
data <- read.dta('./real_world_data_application/Voigtlaender2014.dta')
data <- data[order(data$Sector, data$year), ]
data[, 'const'] <- 1


# variable list of all variables used in table 3
varlist <-
  c(
    'weight_emp',
    'h',
    's_h_avg',
    's2d_h_avg',
    'equip_pw',
    'OCAMovK',
    'HT_diff',
    'RD_int_lag',
    'Outs_na',
    'Outs_diff'
  )


##############################################
# replicate results from voigtlaender table 3
##############################################

# Create weighted data by multiplying each observation by emp_weight. While we could 
# use the weight argument from the plm package, by doing the weighting manually and 
# showing the equivalence of the results, we show that we can just use the manually
# weighted data in the context of the interactive model (in the phtt package is no
# weight argument!).
data_weighted <- data[, 3:452] * sqrt(data$weight_emp)
data_weighted <- cbind(data[, c('Sector', 'year')], data_weighted)

# time dummy creation. While we just could specify twoways fixed effects within plm
# the estimates from voiglaender also rely on dummy variables in the case of time
# fixed effects. Due to dgf adjustment in the manual case the estimates wouldnt be 
# the same.
time_dummies <- "dummy_t1"

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
}


# regressions from voiglaender table 3

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



########################################
# testing and applying interactive model
########################################

# dimension<ality of data 
t <- length(unique(data$year))
i <- length(unique(data$Sector))
p <- length(varlist)

# get all regressors from data.frame format in TxNxp array for applying the phtt package
vars <- array(data = NA, dim = c(t, i, p))
vars_weighted <- array(data = NA, dim = c(t, i, p))

dimnames(vars)[[3]] <- varlist
dimnames(vars_weighted)[[3]] <- varlist

for (k in varlist) {
  for (j in 1:i) {
    vars[1:t, j, k] <- data[(t * (j - 1) + 1):(t * j), k]
  }
  if (k != 'weight_emp') {
    vars_weighted[, , k] <- vars[, , k] * sqrt(vars[, , 'weight_emp'])
  }
}

# implement test for interactive fixed effects like desribes in Liebl paper p. 27/28
# this is actually the test supposed by Kneip / Sickles / Song (2012) to test for
# the presence of interactive FE

## hausman test proposed by BAI

# Model under the null Hypothesis:
twoways.obj <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  factor.dim = 0, 
  additive.effects = "twoways"
  )

# Model under the alternative Hypothesis:
not.twoways.obj <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  factor.dim = 2, 
  additive.effects = "none"
  )

# Hausman test:
checkSpecif(obj1 = twoways.obj, obj2 = not.twoways.obj, level = 0.01)

# test proposed by KNEIP for the model of Bai
# intialize additive model
test_model <-
  Eup(
    vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'twoways'
  )
# test for interactive FE, H0: additive model --> clearly rejects H0
checkSpecif(test_model, level = 0.01)

# estimate new model 

# column 3
weighted_interactive3 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg']
)

summary(weighted_interactive3)

# column 4
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff']
)

summary(weighted_interactive4)

# column 5
weighted_interactive5 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'] + vars_weighted[,,'RD_int_lag'] + vars_weighted[,,'Outs_na'] + vars_weighted[,,'Outs_diff']
)

summary(weighted_interactive5)

# column 6
weighted_interactive6 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's2d_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'] + vars_weighted[,,'RD_int_lag'] + vars_weighted[,,'Outs_na'] + vars_weighted[,,'Outs_diff']
)

summary(weighted_interactive6)


########################################
# comparison table original / interactive
########################################

comparison_table <- data.frame(matrix(NA, nrow = 7, ncol = 10))
colnames(comparison_table) <- c('1_orig', '2_orig', '3_orig', '1-3_interactive', 
                                '4_orig', '4_interactive', '5_orig', '5_interactive', 
                                '6_orig', '6_interactive')

comparison_table[1, 1] <- weighted_1$coefficients
comparison_table[1, 2] <- weighted_2$coefficients
comparison_table[1, 3] <- weighted_3$coefficients[1]
comparison_table[1:4, 5] <- weighted_4$coefficients[1:4]
comparison_table[1:7, 7] <- weighted_5$coefficients[1:7]
comparison_table[1:7, 9] <- weighted_6$coefficients[1:7]

comparison_table[1, 4] <- weighted_interactive3$slope.para
comparison_table[1:4, 6] <- weighted_interactive4$slope.para
comparison_table[1:7, 8] <- weighted_interactive5$slope.para
comparison_table[1:7, 10] <- weighted_interactive6$slope.para


write.xlsx(comparison_table, './output/comp_table_voiglaender.xlsx')

stargazer(weighted_1, weighted_2, weighted_3, weighted_4, weighted_5, weighted_6,
          title = "Results",
          align = TRUE,
          keep = c("s_h_avg", "s2d_h_avg", "equip_pw", "OCAMovK", "HT_diff", "RD_int_lag", "Outs_na", "Outs_diff")
          )



########################################
# appendix
########################################


# just to show equivalence of weighting my build in function or manual adjustment
unweighted_3 <-
  plm(
    h ~ s_h_avg + dummy_t1 + dummy_t2 + dummy_t3 +
      dummy_t4 + dummy_t5 + dummy_t6 + dummy_t7 + dummy_t8 + dummy_t9 + dummy_t10 +
      dummy_t11 + dummy_t12 + dummy_t13 + dummy_t14 + dummy_t15 + dummy_t16 +
      dummy_t17 + dummy_t18 + dummy_t19 + dummy_t20 + dummy_t21 + dummy_t22 +
      dummy_t23 + dummy_t24 + dummy_t25 + dummy_t26 + dummy_t27 + dummy_t28 +
      dummy_t29 + dummy_t30 + dummy_t31 + dummy_t32 + dummy_t33 + dummy_t34 +
      dummy_t35 + dummy_t36 + dummy_t37 + dummy_t38 + dummy_t39 + dummy_t40 +
      dummy_t41 + dummy_t42 + dummy_t43 + dummy_t44 + dummy_t45 + dummy_t46 +
      dummy_t47 + dummy_t48,
    weights = data$weight_emp,
    data = data,
    effect = 'individual',
    model = 'within',
    index = c('Sector', 'year')
  )
coeftest(unweighted_3, vcov = vcovHC(weighted_3, type = "HC0", cluster = "group"))

########################################
# NA data reconstruction
########################################

summary(data[, varlist]) # only NAs in "Outs_na", "Outs_diff" 
data[which(is.na(data[, "Outs_diff"])), c("Sector", "year")] # NAs are not the same
data[which(is.na(data[, "Outs_na"])), c("Sector", "year")]

unique(data[which(is.na(data[, "Outs_na"])), "year"])

########################################
# R^2 computation for original models
########################################

# equivalent to asorb in stata?

# column 3

r2_weighted3 <-
  lm(
    reformulate(paste('s_h_avg', time_dummies, sep = '+'), 'h'),
    data = data_weighted
  )

r2_weighted3 <-
  felm(
    h ~ s_h_avg | Sector,
    data = data_weighted
  )


