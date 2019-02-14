# This function computes the factor and factor
# loadings from the residuals by using PCA
# analysis. 

factor_est <- function(W, R) {
  
  # Get dimension of W.
  c(T, N) %<-% dim(W)
  
  # Check wheter T < N or N >= T, this information will
  # help to speed things up in the computation.
  if (T < N) {
    # In the case of T < N, it is more efficient to 
    # first estimate the factors, from which the 
    # loadings can be inferred. See page 34 of 
    # supplementary material of Bai (2009).
    
    # Compute WW matrix.
    WW <- W %*% t(W) / (T * N)
    # Get eigenvectors and -values
    ev <- eigen(WW) 
    # Compute factors by sclaing R largest eigenvectors by sqrt(T).
    fac_hat <- ev$vectors[, 1:R] * sqrt(T) 
    # Compute laodings from estimated factors. 
    load_hat <- t(W) %*% fac_hat / T
  }  else {
    # Compute WW matrix.
    WW <- t(W) %*% W / (T * N) 
    # Get eigenvectors and -values.
    ev <- eigen(WW)
    # Compute loadings by sclaing R largest eigenvectors by sqrt(T).
    load_hat <- ev$vectors[, 1:R] * sqrt(N)
    # Compute factors from estimated loadings. 
    fac_hat <- W %*% load_hat / N
  }
  
  # Compute eigenvalues for completeness, although not necessarily
  # needed for IFE estimator.
  eigenvalues <- diag(ev$values[1:R])
  
  # Return factors, loadings and eigenvalues.
  return(list(fac_hat, load_hat, eigenvalues)) 
  
}
