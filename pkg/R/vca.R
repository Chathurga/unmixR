##' Vertex Component Analysis unmixing algorithm
##' 
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##' @param p Number of endmembers
##' @return Endmember indices
##' 
##' @references Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol.43, no.4, pp.898,910, April 2005;
##'   doi: 10.1109/TGRS.2005.844293

vca <- function(data, p) {
  # get the matrix representation of the input (if it's some other class)
  data <- as.matrix(data)
  
  # BH: I think the language here is sort of reversed: an entire spectrum
  # is a sample in the usual lingo.  In my mind nspectra is the no. of 
  # samples. I'd prefer to call ncol(data) nfreq for no of frequencies
  # or if you want to stick close to the nomenclature in the VCA paper
  # I think it would be L (if you call it something else, identify it as
  # L in the paper)
  nspectra <- nrow(data)
  nsamples <- ncol(data)
  
  # BH: estimating SNR is more complex than it seems: you need to find
  # 'peaks' and 'noise' separately, then ratio them.  The problem
  # comes in identifying peaks, which by definition exceed the noise by
  # some threshold.  Determining the threshold is tricky.
  # Package ChemometricsWithR has some peak finding functions
  # and I have adapted a few of them - let me know if they are of interest.
  
  # estimate the signal to noise ratio
  rowMean <- apply(data, 1, mean) # get the mean of each row
  # repeat the column of row means so that it matches the size of the data
  repMean <- as.matrix(rowMean)[, rep(1, nsamples)]
  zMean <- data - repMean # zero mean the data
  
  Ud <- svd(tcrossprod(zMean) / nsamples, nv=p)$v
  E <- function(M, n) sum(c(M)^2 / n) # expectation operator
  pr <- E(data, nsamples)
  prp <- E(crossprod(Ud, zMean), nsamples) + crossprod(rowMean)
  SNR <- 10 * log10((prp - (p / nspectra) * pr) / (pr - prp))
  
  # signal to noise threshold # BH: this is the initial, crude SNR and
  # I would suggest computing it earlier to make it clear.
  # This value will certainly be replaced on the first iteration.
  
  SNRth <- 15 + 10 * log10(p)
  # if the estimated SNR is over a certain threshold ...
  if (SNR > SNRth) {
    d <- p
  } else {
    d <- p - 1
  }
  
  SNR
}