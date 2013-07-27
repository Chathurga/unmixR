##' Harsanyi-Farrand-Chang method for estimating the number of endmembers in
##' a spectral image
##' 
##' Calculates the difference between the eigenvalues in the correlation and
##' covariance matrices of the spectra to determine the virtual
##' dimensionality (VD)
##' 
##' @param data Spectra matrix with spectra in rows, frequencies in columns.
##' @param alpha False-alarm probability (can be a vector of alpha values)
##' @return The estimated number of endmembers (in the form vector if alpha
##'   is a vector)
##'
##' @Reference C.-I. Chang and Q. Du "Estimation of Number of Spectrally Distinct
##' Signal Sources in Hyperspectral Imagery" IEEE Transactions on GeoScience and
##' Remote Sensing vol. 42, no. 3, 2004.
##'
##' @Reference P. Bajorski "Second Moment Linear Dimensionality as an Alternative to
##' to Virtual Dimensionality" IEEE Transactions on GeoScience and
##' Remote Sensing vol. 49, no. 2, 2011.
##'
##' @Reference I. Gerg "An Evaluation of Three Endmember Extraction Algorithms:
##' ATGP, ICA-EEA, and VCA" Section 2.1 in particular
##' MS thesis, Pennsylvania State Univ., 2008.
##'
##' @Details In this algorithm the definitions of covariance and correlation
##' matrices are NOT those of statisticians, but those of electrical engineers.
##' This is pointed out in Bajorski. Gerg also makes this clear.

hfcS <- function(data, alpha) {

  # This version works on the samples.  See also hfcVAR
  
  # Warning: covariance and correlation usage conforms to the EE definitions
  # not the standard statistical definitions
  
  ns <- dim(data)[1] # number of samples
  
  # R is the sample correlation matrix (nomenclature same as in references)
  # R & K (below) will have dimensions ns x ns
  R <- tcrossprod(data)/ns

  # K is the sample covariance matrix, called R in Bajorski, K in Chang
  cM <- colMeans(data)
  K <- ((data - cM) %*% t(data - cM))/ns

  # these next variables are vectors w/length ns

  R.ev <- eigen(R)$values
  K.ev <- eigen(K)$values
  diff.ev <- R.ev - K.ev

  # calculate variance of each row of the original data matrix
  # i.e. the variance of a given sample spectrum
  
  variance <- apply(data, MARGIN = 1, FUN = var)

  # now estimate endmembers for each value of alpha supplied
  
  tau <- lapply(alpha, FUN = qnorm, mean = 0, sd = variance, lower.tail = FALSE)
  p <- unlist(lapply(tau, FUN = function(x) {sum(diff.ev > x)}))
  p
  }