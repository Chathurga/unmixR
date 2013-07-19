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
  transpose <- t(data) #CB: I suspect that this should not be transposed
  
  dims <- dim(data)
  nvariables <- dims[1]
  nsamples <- dims[2]
  
  R <- (data %*% transpose) / nsamples #CB: have a look at ?crossprod and ?cor
  K <- cov(transpose)
  
  lcorr <- eigen(R)$values
  lcov <- eigen(K)$values
  
  variance <- sqrt(2*(lcorr^2 + lcov^2) / nsamples)
  difference <- lcorr - lcov
  
  as.numeric(Map(function(a) {
    tau <- -qnorm(a, array(0, nvariables), variance)
    sum(difference > tau)
  }, alpha))
}