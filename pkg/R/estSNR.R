##' Estimates Signal to Noise Ratio
##' 
##' Estimate the signal to noise ratio of a hyperspectral image
##' 
##' @references Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol.43, no.4, pp.898,910, April 2005;
##'   doi: 10.1109/TGRS.2005.844293

estSNR <- function(data, p) {
  E <- function(M, n) sum(c(M)^2 / n) # expectation operator
  
  nsamples <- ncol(data)
  rowMean <- apply(data, 1, mean) # get the mean of each row
  # repeat the column of row means so that it matches the size of the data
  repMean <- repvec.col(rowMean, nsamples)
  zMean <- data - repMean # zero mean the data
  Ud <- svd(tcrossprod(zMean) / nsamples, nv=p)$u[,1:p]
  zProj <- crossprod(Ud, zMean) # project the zero mean data
  
  pr <- E(data, nsamples)
  prp <- E(crossprod(Ud, zMean), nsamples) + crossprod(rowMean)
  SNR <- 10 * log10((prp - (p / nspectra) * pr) / (pr - prp))
  
  SNR
}