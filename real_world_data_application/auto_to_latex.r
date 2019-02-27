get_significance <- function(coeff_rows, latex_table) {
  for (row in coeff_rows) {
    for (col in 1:ncol(latex_table)) {
      if (latex_table[row + 1, col] * 2.69 > latex_table[row, col]) {
        latex_table[row, col] <-
          sprintf("$%s^{***}$", latex_table[row, col])
      } else if ((latex_table[row + 1, col] * 1.96 > latex_table[row, col])) {
        latex_table[row, col] <-
          sprintf("$%s^{**}$", latex_table[row, col])
      } else if ((latex_table[row + 1, col] * 1.69 > latex_table[row, col])) {
        latex_table[row, col] <-
          sprintf("$%s^{*}$", latex_table[row, col])
      } else {
        latex_table[row, col] <-
          sprintf("$%s^{}$", latex_table[row, col])
      }
    }
  }
  return(latex_table)  
}

# Read in coefficient and se tables.
coeffs <- readRDS()
stand_errs <- readRDS()

c(nrow, ncol) %<-% dim(coeffs)

# Construct dataframe of dimensions from coeff_df.
latex_table <-
  data.frame(matrix(
    data = NA,
    nrow = nrow * 3,
    ncol = ncol
  ))

# Print SE to final table.
se_rows <- seq(from = 2, to = 21, by = 3)
latex_table[se_rows, ] <- sprintf("(%s)", stand_errs)

# Print regression coefficients to final table with respective sign. stars.
coeff_rows <- seq(from = 1, to = 21, by = 3)
latex_table[coeff_rows] <- coeffs
latex_table <- get_significance(coeff_rows, latex_table)

# Write to csv.
write.csv(latex_table, ".csv")