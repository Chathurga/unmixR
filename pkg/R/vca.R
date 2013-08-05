##' Vertex Component Analysis unmixing algorithm
##' 
##' -- DESCRIPTION --
##' 
##' @references Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol.43, no.4, pp.898,910, April 2005;
##'   doi: 10.1109/TGRS.2005.844293

vca <- function(data, p) {
  # get the matrix representation of the input (if it's some other class)
  data <- as.matrix(data)
  
  nspectra <- nrow(data)
  nsamples <- ncol(data)
  
  SNRth <- 15 + 10 * log10(p)
  
  if (SNR > SNRth) {
    d <- p
  } else {
    d <- p - 1
  }
}