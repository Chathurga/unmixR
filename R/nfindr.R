##' N-FINDR unmixing algorithm
##' 
##' This technique is based on the fact that in N spectral dimensions, the
##' N-volume contained by a simplex formed of the purest pixels is larger
##' than any other volume formed from any other combination of pixels.
##' 
##' @param data Data to unmix. Will be converted to a matrix using as.matrix.
##' @param p Number of endmembers (will be estimated using \code{\link{hfc}}
##'   if omitted)
##' @param iters Max number of iterations (defaults to 3 * p)
##' @return Returns a (p x no. of spectra) matrix where each row is a
##'   pure component
##' @references Michael E. Winter; "N-FINDR: an algorithm for fast autonomous
##'   spectral end-member determination in hyperspectral data", Proc.
##'   SPIE 3753, Imaging Spectrometry V, 266 (October 27, 1999);
##'   doi:10.1117/12.366289;

nfindr <- function(data, p=hfc(data, 10^(-5)), iters=3*p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  # reduce the dimensionality of the input (to p-1) using PCA
  reduced <- prcomp(data)$x[,1:(p-1),drop=F]
  
  # select random indexes that form the initial simplex
  # this simplex will be inflated until the pure pixels are found
  indexes <- sample(nrow(data), p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indexes,]
  
  # calculate the initial volume using the random endmembers
  volume <- abs(det(simplex))
  it <- 1
  v1 <- -1
  v2 <- volume
  
  # keep replacing endmembers until there is never an increase in volume
  # or the max iterations are reached (indicates pure endmembers not found)
  while (v2 > v1 && it <= iters) {
    for (k in 1:p) {
      for (i in 1:nspectra) {
        # store current sample as it may need to be placed back into the
        # simplex after the following replacement
        sample <- simplex[2:p,k]
        
        # replace the k-th endmember with the i-th reduced spectrum
        # and recalculate the volume
        simplex[2:p,k] <- reduced[i,]
        testVolume <- abs(det(simplex))
        
        # if the replacement increased the volume then keep the replacement
        # and the note the spectrum's index
        if (testVolume > volume) {
          volume <- testVolume
          indexes[k] <- i
        }
        # otherwise revert the replacement
        else {
          simplex[2:p,k] <- sample
        }
      }
    }
    
    it <- it+1
    v1 <- v2
    v2 <- volume
  }
  
  # return the spectra that increased the simplex volume the most
  E <- data[indexes,]
  C <- array(0, p)
  C[indexes] <- 1
  
  E
}
