# Used packages.
library(foreign)
library(xlsx)
library(zeallot)

# Read in data provided by Voigtlaender.
data <- read.dta('./data_application/original_data/Voigtlaender2014.dta')

# Read in sic-codes containing real names of sectors.
sic_codes <-
  read.xlsx('./data_application/original_data/sic_names.xlsx', sheetIndex = 1)
sic_two_digit <-
  read.xlsx('./data_application/original_data/sic_overreaching_sectors.xlsx',
            sheetIndex = 1)

# Order data.
data <- data[order(data$Sector, data$year),]

# Add further variables that could be of interest.
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
    'h_lag',
    's_h',
    'sig_h',
    'tau_h',
    'rho_h',
    'sig_N2d_h',
    'tau_N2d_h',
    'rho_N2d_h',
    's_h_w_avg80'
  )

# Create weighted data by multiplying each observation by emp_weight. While we could
# use the weight argument from the plm package, by doing the weighting manually and
# showing the equivalence of the results, we show that we can just use the manually
# weighted data in the context of the interactive model (in particular, there is no
# weight argument in the phtt package).
data_weighted <- data[, 3:451] * sqrt(data$weight_emp)
data_weighted <-
  cbind(data[, c('Sector', 'year', 'const', 'trend', 'sector', 'h_lag')], data_weighted)

# Time dummy creation. While we just could specify twoways fixed effects within plm
# the estimates from Voiglaender also rely on dummy variables in the case of time
# fixed effects. Due to degree of freedom adjustments in the manual case the estimates wouldn't be
# the same.
time_dummies <- "dummy_t1"
varlist <- append(varlist, "dummy_t1")

for (i in 2:48) {
  temp_dummy <- paste("dummy_t", i, sep = "")
  time_dummies <- paste(time_dummies, temp_dummy, sep = "+")
  varlist <- append(varlist, paste("dummy_t", i, sep = ""))
}

# Get panel to matrix form to apply phtt.
t <- length(unique(data$year))
i <- length(unique(data$Sector))
p <- length(varlist)

# Get all regressors from dataframe format in TxNxp array for applying the phtt package.
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
        vars_weighted[, , k] <- vars[, , k]
      } else{
        vars_weighted[, , k] <- vars[, , k] * sqrt(vars[, , 'weight_emp'])
      }
    }
  }
}

# Dataframe and matrix data with balanced sectors.
balanced_sectors <-
  unique(data_weighted[which(is.na(data_weighted['Outs_na'])), 'sector'])

data_weighted_bal <-
  data_weighted[which(!data_weighted$sector %in% balanced_sectors), ]

vars_weighted_bal <- vars_weighted[, -c(balanced_sectors), ]

# Save panel data.
saveRDS(data_weighted,
        './data_application/original_data/data_weighted.rds')
saveRDS(data_weighted_bal,
        './data_application/original_data/data_weighted_bal.rds')

# Save matrix-form data.
saveRDS(vars_weighted,
        './data_application/original_data/vars_weighted.rds')
saveRDS(vars_weighted_bal,
        './data_application/original_data/vars_weighted_bal.rds')