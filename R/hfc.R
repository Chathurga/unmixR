##' Harsanyi-Farrand-Chang method for estimating the number of endmembers
##' 
##' (-description-)
##' 
##' @param data Spectra matrix to examine
##' @param alpha False-alarm probability
##' @return (-return-)

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
  vd <- array(0, p);
  
  for (i in 1:p) {
      tau <- -qnorm(alpha[i], array(0, nvariables), variance)
      vd[i] <- sum(difference > tau)
  }
  
  vd
}