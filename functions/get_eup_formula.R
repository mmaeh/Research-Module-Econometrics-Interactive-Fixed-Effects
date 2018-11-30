get_matrix_formula <-
  function(dependent, covariates) {
    covariates_tr <-
      sapply(covariates, function(x)
        paste("vars_weighted[,,'", x, "']", sep = ''))
    
    covariates_tr <- paste(covariates_tr, collapse = '+')
    
    string <-
      paste(paste("vars_weighted[,,'",
                  dependent,
                  "']", sep = ''),
            covariates_tr,
            sep = '~')
    
    return(as.formula(string))
  }

get_matrix_formula('hallo', c('penis', 'lustig'))

