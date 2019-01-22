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
    WW <- W %*% t(W) / (T * N)
    ev <- eigen(WW) # Get eigenvectors and -values.
    fac_hat <- ev$vectors[, 1:R] * sqrt(T) # Compute factors by sclaing R largest eigenvectors by sqrt(T).
    load_hat <- t(W) %*% fac_hat / T # Compute laodings from estimated factors. 
  }  else {
    WW <- t(W) %*% W / (T * N) 
    ev <- eigen(WW) # Get eigenvectors and -values.
    load_hat <- ev$vectors[, 1:R] * sqrt(N) # Compute loadings by sclaing R largest eigenvectors by sqrt(T).
    fac_hat <- W %*% load_hat / N # Compute factors from estimated loadings. 
  }
  
  # Compute eigenvalues for completeness.
  eigenvalues <- diag(ev$values[1:R])
  
  # Return factors, loadings and eigenvalues.
  return(list(fac_hat, load_hat, eigenvalues)) 
  
}
