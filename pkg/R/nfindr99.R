##' Michael E. Winter's 1999 N-FINDR unmixing algorithm
##' 
##' This method should only be called from \code{\link{nfindr}}.
##' This technique is based on the fact that, in N spectral dimensions, the
##' N-volume contained by a simplex formed of the purest pixels is larger
##' than any other volume formed from any other combination of pixels.
##' 
##' @param data Data matrix to unmix
##' @param p Number of endmembers
##' @param simplex The initial simplex estimation
##' @param indices Indices used in the simplex estimation
##' @param iters Max number of iterations
##' @include unmixR-package.R
##' @return The indices that indicate the position of the endmembers in the
##'   original dataset
##' 
##' @references Michael E. Winter; "N-FINDR: an algorithm for fast autonomous
##'   spectral end-member determination in hyperspectral data", Proc.
##'   SPIE 3753, Imaging Spectrometry V, 266 (October 27, 1999);
##'   doi:10.1117/12.366289;

nfindr99 <- function(data, p, simplex, indices, iters) {
  nspectra <- nrow(data)
  
  # calculate the initial volume using the random endmembers
  volume <- abs(det(simplex))
  volume.prev <- -1
  volume.now <- volume
  
  # keep replacing endmembers until there is never an increase in volume
  # or the max iterations are reached (indicates pure endmembers not found)
  iter <- 1
  while (volume.now > volume.prev && iter <= iters) {
    for (k in 1:p) {
      for (i in 1:nspectra) {
        # store current sample as it may need to be placed back into the
        # simplex after the following replacement
        sample <- simplex[2:p,k]
        
        # replace the k-th endmember with the i-th reduced spectrum
        # and recalculate the volume
        simplex[2:p,k] <- data[i,]
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
    
    iter <- iter+1
    volume.prev <- volume.now
    volume.now <- volume
  }
  
  indices
}

.test (nfindr99) <- function (){
  e <- nfindr99 (data = diag (3), p = 2, simplex = rbind(rep(1, 3), diag (3)[1:2,]),
                 iters = 0, i = 1 : 2)
  checkTrue (e == 1 : 2)

}
