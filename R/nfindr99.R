##' Michael E. Winter's 1999 N-FINDR unmixing algorithm
##' 
##' This technique is based on the fact that in N spectral dimensions, the
##' N-volume contained by a simplex formed of the purest pixels is larger
##' than any other volume formed from any other combination of pixels.
##' 
##' @param data Data to unmix. Will be converted to a matrix using as.matrix.
##'   The matrix should contain a spectrum per row.
##' @param p Number of endmembers (will be estimated using \code{\link{hfc}}
##'   if omitted)
##' @param iters Max number of iterations (defaults to 3 * p)
##' @return A structure which contains:
##'   \itemize{
##'     \item \strong{data}: the original data (or NULL if the input data was
##'                          already reduced)
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers
##'   }
##' @references Michael E. Winter; "N-FINDR: an algorithm for fast autonomous
##'   spectral end-member determination in hyperspectral data", Proc.
##'   SPIE 3753, Imaging Spectrometry V, 266 (October 27, 1999);
##'   doi:10.1117/12.366289;

nfindr99 <- function(data, p, iters=3*p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  reduced <- data  #CB maybe use data all the way -> see comment on return value 
  if (ncol (data) != p - 1) { #CB variates are in columns
    reduced <- prcomp(data)$x[,1:(p-1),drop=F]
  }
  
  # select random indices that form the initial simplex
  # this simplex will be inflated until the pure pixels are found
  indices <- sample(nrow(reduced), p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]
  
  # calculate the initial volume using the random endmembers
  volume <- abs(det(simplex))
  it <- 1
  v1 <- -1 #CB maybe rename to volume.lastit
  v2 <- volume  #CB maybe rename to volume.thisit
  
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
          indices[k] <- i
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
  
  
  #CB: return only indices, the structure can be constructed in nfinder.default (the wrapper for all N-FINDR methods)
  structure(list(
    data = if (!identical(data, reduced)) data else NULL,
    indices = indices
  ), class = "nfindr")
}
