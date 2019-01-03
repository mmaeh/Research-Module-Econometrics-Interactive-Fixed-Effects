library(foreign)
library(ggplot2)
library(gplots)
library(lattice)
library(lfe)
library(lmtest)
library(multiwayvcov)
library(phtt)
library(plm)
library(plotly)
library(psych)
library(rgl)
library(stargazer)
library(xlsx)
library(zeallot)

# read in data from voigtlaender
data <- read.dta('./real_world_data_application/Voigtlaender2014.dta')
data <- data[order(data$Sector, data$year), ]
data[, 'const'] <- 1
data[, 'trend'] <- rep(1:48, times = 358)
data[, 'h_lag'] <-
  unlist(tapply(data$h, data$Sector, function(x) {
    c(NA, x[-length(x)])
  }))

sic_codes <- read.xlsx('./real_world_data_application/sic_names.xlsx', sheetIndex = 1)
sic_overreaching_sectors <- read.xlsx('./real_world_data_application/sic_overreaching_sectors.xlsx', sheetIndex = 1)

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
    'Outs_diff',
    'trend',
    'h_lag'
  )


##############################################
# replicate results from voigtlaender table 3
##############################################

# Create weighted data by multiplying each observation by emp_weight. While we could 
# use the weight argument from the plm package, by doing the weighting manually and 
# showing the equivalence of the results, we show that we can just use the manually
# weighted data in the context of the interactive model (in the phtt package is no
# weight argument!).
data_weighted <- data[, 3:454] * sqrt(data$weight_emp)
data_weighted <- cbind(data[, c('Sector', 'year')], data_weighted)

balanced_sectors <- unique(data_weighted[which(is.na(data_weighted['Outs_na'])),"Sector"])
data_weighted_bal <- data_weighted[which(!data_weighted[,'Sector'] %in% balanced_sectors),]

# time dummy creation. While we just could specify twoways fixed effects within plm
# the estimates from voiglaender also rely on dummy variables in the case of time
# fixed effects. Due to dgf adjustment in the manual case the estimates wouldnt be 
# the same.
time_dummies <- "dummy_t1"
varlist <- append(varlist, "dummy_t1")

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
  varlist <- append(varlist, paste("dummy_t", i, sep = ""))
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
    if (k != 'weight_emp') {
      if (k == 'trend') {
        vars_weighted[, , k] <- vars[,,k]
      } else{
        vars_weighted[, , k] <- vars[, , k] * sqrt(vars[, , 'weight_emp'])
      }
    }
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
    additive.effects = 'individual'
  )
# test for interactive FE, H0: additive model --> clearly rejects H0
checkSpecif(test_model, level = 0.01)

# estimate new model 

# column 3
weighted_interactive3 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'],
  additive.effects = 'twoways',
  dim.criterion = 'IPC1'
)

summary(weighted_interactive3)

# column 4
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  additive.effects = 'twoways',
  dim.criterion = 'IPC1'
)

plot(summary(weighted_interactive4))

opt_obj <- OptDim(weighted_interactive4, criteria = c("PC3", "ER", "GR", "IPC1", "IPC2", "IPC3"))

plot(opt_obj)


# column 5
weighted_interactive5 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'] + vars_weighted[,,'RD_int_lag'] + vars_weighted[,,'Outs_na'] + vars_weighted[,,'Outs_diff']
)

summary(weighted_interactive5)

# column 6
weighted_interactive6 <- Eup(
  vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's2d_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'] + vars_weighted[,,'RD_int_lag'],
  additive.effects = 'twoways',
  dim.criterion = 'IPC1'  
)

summary(weighted_interactive6)
plot(summary(weighted_interactive6))

opt_obj <- OptDim(weighted_interactive6)
plot(opt_obj)

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


x <- matrix(rep(1:48, times = 358), nrow = 48, ncol = 358)

vars_weighted[,,'trend'] <- matrix(rep(1:48, times = 358), nrow = 48, ncol = 358)



weighted_interactive4 <- Eup(
  get_matrix_formula("h", c("s_h_avg", "equip_pw", "OCAMovK", "HT_diff", "trend")),
  additive.effects = 'individual',
  dim.criterion = 'IPC2'
)


data_weighted <- data_weighted[order(data_weighted$Sector, data_weighted$year), ]

data_weighted[,'resid'] <- sapply(seq(1:17184), function(x) weighted_4$residuals[[x]]) 
data_weighted[,'resid_stan'] <- sapply(data_weighted['resid'], function(x) (x - mean(data_weighted[,'resid'])) / sd(data_weighted[,'resid']))
data_weighted[,'resid_stan'] <- abs(data_weighted[,'resid_stan'])
data_weighted[,'fitted'] <- sapply(seq(1:17184), function(x) (weighted_4$model[[1]][[x]] - weighted_4$residuals[[x]]))
ggplot(data_weighted, aes(x=fitted,y=resid_stan)) +
  geom_point(aes(color=name.x, shape = name.x))+geom_hline(yintercept=0)+geom_smooth() +
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector)
ggplot(data_weighted, aes(x=sector, y=year)) +
  geom_tile(aes(fill=resid_stan)) +theme_bw() + coord_equal() +
  scale_fill_gradientn(colours = c("yellow", "green", "blue", "green", "yellow"),
                       values = rescale(c(-6,-3,0,3,6)), guide = "colorbar",limits=c(-6,6))
bwr.colors <- colorRampPalette(c("yellow", "green", "blue", "green", "yellow"))



data_weighted[,'resid_inter'] <- as.vector(weighted_interactive4$residuals)
data_weighted[,'resid_inter_stan'] <- sapply(data_weighted['resid_inter'], function(x) (x - mean(data_weighted[,'resid_inter'])) / sd(data_weighted[,'resid_inter']))
data_weighted[,'resid_inter_stan'] <- abs(data_weighted[,'resid_inter_stan'])
data_weighted[,'fitted_inter'] <- as.vector(weighted_interactive4$fitted.values)
data_weighted[,'sector'] <- rep(1:358, each = 48)
ggplot(data_weighted, aes(x=fitted_inter,y=resid_inter_stan)) +
  geom_point(aes(color=name.x, shape = name.x))+geom_hline(yintercept=0)+geom_smooth() +
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector)
ggplot(data_weighted, aes(x=sector, y=year)) +
  geom_tile(aes(fill=resid_inter_stan)) +theme_bw() + coord_equal() +
  scale_fill_gradientn(colours = c("yellow", "green", "blue", "green", "yellow"),
                       values = rescale(c(-6,-3,0,3,6)), guide = "colorbar",limits=c(-6,6))


resid1_inter(weighted_interactive4, "w4_inter_resid1")

resid <- as.vector(weighted_interactive4$residuals)
resid <- sapply(resid, function(x) (x - mean(resid)) / sd(resid))
data_weighted['x'] <- qnorm(ppoints(resid))
ggplot(data_weighted, aes(x=x, y=resid_inter_stan)) +
  geom_point() +
  geom_line(aes(x=x, y=x)) 


ggplot(data_weighted, aes(x=year, y=h, colour = factor(Sector))) +
  geom_point()

p <- plot_ly(x = data_weighted$year, y = data_weighted$Sector, z = data_weighted$resid_stan) 
add_mesh(p)


results_1 <- matrix(NA, nrow = 4, ncol = 12)

for (i in 1:6) {
  eup_model <- summary(weighted_interactive4 <- Eup(
    vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'individual',
    factor.dim = i
  ))
  
  results_1[, ((i-1)*2 + 1):(i*2)] <- eup_model$coefficients[, 1:2]
}


results_2 <- matrix(NA, nrow = 5, ncol = 12)

for (i in 1:6) {
  eup_model <- summary(weighted_interactive4 <- Eup(
    vars_weighted[, , 'h'] ~ -1 + vars_weighted[, , 'h_lag'] +vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
    additive.effects = 'individual',
    factor.dim = i
  ))
  
  results_2[, ((i-1)*2 + 1):(i*2)] <- eup_model$coefficients[, 1:2]
}    


residuals <- weighted_interactive4$residuals
factors <- weighted_interactive4$unob.factors
loadings <- weighted_interactive4$ind.loadings

sectors <- as.data.frame(unique(data['Sector']))
structure <- as.data.frame(t(weighted_interactive4$unob.fact.stru)[,1])
struc_by_sectors <- cbind(sectors, structure)
colnames(struc_by_sectors) <- c('sector', 'structure for t == 1')
struc_by_sectors <- merge(struc_by_sectors, sic_codes, by = 'sector', all.x = TRUE)

write.xlsx(struc_by_sectors, './output/struc_by_sectors.xlsx')


residuals <- as.vector(weighted_interactive4$residuals)
residuals_stan <- sapply(residuals, function(x) (x - mean(residuals)) / sd(residuals))
residuals_stan <- matrix(residuals_stan, nrow = 48, ncol = 358)


heatmap(residuals_stan, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
ggplot(hm, aes(x=x, y=y, fill=value))


data_weighted['two_digit_code'] <- sapply(data_weighted['Sector'], function(x) substr(x, start = 1, stop = 2))
data_weighted['fac_str'] <- as.vector(weighted_interactive4$unob.fact.stru)
data_weighted <- merge(data_weighted, sic_overreaching_sectors, by.x = "two_digit_code", by.y = "code")
data_weighted['code_1'] <- as.factor(data_weighted[,'code_1'])
col_vector <- c('blue', 'red', 'purple', 'blue', 'red', 'purple', 'blue', 'red', 'purple', 'blue', 'red', 'purple', 'black')
line_vector <- c('solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed')
shape_vector <- c(15,15,15,16,16,16,15,15,15,16,16,16,15)
name_vector <- as.character(unique(data_weighted$name_1))
ggplot(data = data_weighted, aes(x = year, y = fac_str, group = Sector, linetype = code_1, color = code_1, shape = code_1)) + 
  geom_line() +
  geom_point(size = 3) + 
  scale_colour_manual(values = col_vector, labels = name_vector) + 
  scale_linetype_manual(values = line_vector, labels = name_vector) + 
  scale_shape_manual(values = shape_vector, labels = name_vector) +
  ylab("Factor Structure") + 
  xlab("Year") + 
  labs(color = "Sector", shape = "Sector", linetype = "Sector") + 
  theme_minimal()
ggsave("factor_str.png", dpi = 600, width = 15, height = 10)
