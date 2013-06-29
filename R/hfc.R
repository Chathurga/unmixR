##' Harsanyi-Farrand-Chang method for estimating the number of endmembers in
##' a spectral image
##' 
##' Calculates the difference between the eigenvalues in the correlation and
##' covariance matrices of the spectra to determine the virtual
##' dimensionality (VD)
##' 
##' @param data Spectra matrix to examine
##' @param alpha False-alarm probability (can be a vector of alpha values)
##' @return The estimated number of endmembers (in the form vector if alpha
##'   is a vector)

hfc <- function(data, alpha) {
  transpose <- t(data)
  
  dims <- dim(data)
  nvariables <- dims[1]
  nsamples <- dims[2]
  
  R <- (data %*% transpose) / nsamples
  K <- cov(transpose)
  
  lcorr <- eigen(R)$values
  lcov <- eigen(K)$values
  
  variance <- sqrt(2*(lcorr^2 + lcov^2) / nsamples)
  difference <- lcorr - lcov
  
  p <- length(alpha)
  vd <- array(0, p)
  
  for (i in 1:p) {
      tau <- -qnorm(alpha[i], array(0, nvariables), variance)
      vd[i] <- sum(difference > tau)
  }
  
  vd
}