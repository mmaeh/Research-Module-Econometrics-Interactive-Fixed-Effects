library(fUnitRoots)
library(tseries)
library(phtt)

# Eup model
weighted_interactive4 <- Eup(
  vars_weighted[, , 'h'] ~ vars_weighted[, , 's_h_avg'] + vars_weighted[, , 'equip_pw'] + vars_weighted[, , 'OCAMovK'] + vars_weighted[, , 'HT_diff'],
  factor.dim = 3
)


# resulting unobsvered factors as in Figure 
unob_factors <- weighted_interactive4$unob.factors
blue_factor <- unob_factors[,1]
orange_factor <- unob_factors[,2]
grey_factor <- unob_factors[,3] 

# just to check that we really got the right one
plot(blue_factor)
plot(orange_factor)
plot(grey_factor)

# stationary tests
Box.test(blue_factor, lag = 20, type="Ljung")
kpss.test(blue_factor,null="Level")
kpss.test(blue_factor,null="Trend")
adfTest(blue_factor, lags = 1, type = "nc")
adfTest(blue_factor, lags = 1, type = "c")
adfTest(blue_factor, lags = 1, type = "ct")

Box.test(orange_factor, lag = 20, type="Ljung")
kpss.test(orange_factor,null="Level")
kpss.test(orange_factor,null="Trend")
adfTest(orange_factor, lags = 1, type = "nc")
adfTest(orange_factor, lags = 1, type = "c")
adfTest(orange_factor, lags = 1, type = "ct")

Box.test(grey_factor, lag = 20, type="Ljung")
kpss.test(grey_factor,null="Level")
kpss.test(grey_factor,null="Trend")
adfTest(grey_factor, lags = 1, type = "nc")
adfTest(grey_factor, lags = 1, type = "c")
adfTest(grey_factor, lags = 1, type = "ct")