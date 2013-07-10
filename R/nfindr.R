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

nfindr <- function(data, p=hfc(data, 10^(-5)), iters=3*p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  reduced <- prcomp(data)$x[,1:(p-1)]
  
  testMatrix <- matrix(0, nrow=p, ncol=p)
  testMatrix[1,] <- 1
  
  indexes <- sample(ncol(data), p)
  testMatrix[2:p,] <- reduced[indexes]
  
  volume <- abs(det(testMatrix))
  it <- 1
  v1 <- -1
  v2 <- volume
  
  while (it <= iters && v2 > v1) {
    for (k in 1:p) {
      for (i in 1:nspectra) {
        sample <- testMatrix[2:p,k]
        
        testMatrix[2:p,k] <- reduced[i]
        testVolume <- abs(det(testMatrix))
        
        if (testVolume > volume) {
          volume <- testVolume
          indexes[k] <- i
        } else {
          testMatrix[2:p,k] <- sample
        }
      }
    }
    
    it <- it+1
    v1 <- v2
    v2 <- volume
  }
  
  E <- data[indexes,]
  C <- array(0, p)
  C[indexes] <- 1
  
  E
}
