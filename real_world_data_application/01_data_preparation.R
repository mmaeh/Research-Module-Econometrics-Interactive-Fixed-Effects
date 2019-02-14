library(foreign)
library(xlsx)
library(zeallot)

# Read in data provided by Voigtlaender.
data <- read.dta('./real_world_data_application/Voigtlaender2014.dta')
# Read in sic-codes containing real names of sectors. 
sic_codes <- read.xlsx('./real_world_data_application/sic_names.xlsx', sheetIndex = 1)
sic_two_digit <- read.xlsx('./real_world_data_application/sic_overreaching_sectors.xlsx', sheetIndex = 1)

# Reorder data, just to be sure. 
data <- data[order(data$Sector, data$year), ]

data[, 'const'] <- 1
data[, 'sector'] <- rep(1:358, each = 48)
data[, 'trend'] <- rep(1:48, times = 358)
data[, 'h_lag'] <-
  unlist(tapply(data$h, data$Sector, function(x) {
    c(NA, x[-length(x)])
  }))

# Variable list of all variables used in Table 3 of Voigtlaender.
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
# Replicate results from voigtlaender table 3
##############################################

# Create weighted data by multiplying each observation by emp_weight. While we could 
# use the weight argument from the plm package, by doing the weighting manually and 
# showing the equivalence of the results, we show that we can just use the manually
# weighted data in the context of the interactive model (in the phtt package is no
# weight argument!).
data_weighted <- data[, 3:451] * sqrt(data$weight_emp)
data_weighted <- cbind(data[, c('Sector', 'year', 'const', 'trend', 'sector', 'h_lag')], data_weighted)

# Time dummy creation. While we just could specify twoways fixed effects within plm
# the estimates from Voiglaender also rely on dummy variables in the case of time
# fixed effects. Due to dgf adjustment in the manual case the estimates wouldn't be 
# the same.
time_dummies <- "dummy_t1"
varlist <- append(varlist, "dummy_t1")

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
  varlist <- append(varlist, paste("dummy_t", i, sep = ""))
}


# Get panel in matrix form to apply phtt.
t <- length(unique(data$year))
i <- length(unique(data$Sector))
p <- length(varlist)

# Get all regressors from data.frame format in TxNxp array for applying the phtt package.
vars <- array(data = NA, dim = c(t, i, p))
vars_weighted <- array(data = NA, dim = c(t, i, p))

# Define "column-names" for easier calling in phtt package.
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

# Dataframe in matrix form, only including balanced sectors
balanced_sectors <- unique(data_weighted[which(is.na(data_weighted['Outs_na'])),'sector'])
data_weighted_bal <- data_weighted[which(!data_weighted$sector %in% balanced_sectors),]
vars_weighted_bal <- vars_weighted[,-c(balanced_sectors),]

# Save data.
saveRDS(data_weighted, './real_world_data_application/data_weighted.rds')
saveRDS(data_weighted_bal, './real_world_data_application/data_weighted_bal.rds')
saveRDS(vars_weighted, './real_world_data_application/vars_weighted.rds')
saveRDS(vars_weighted_bal, './real_world_data_application/vars_weighted_bal.rds')