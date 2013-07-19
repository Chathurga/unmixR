nfindrSeqLDU <- function(data, p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  reduced <- prcomp(data)$x[,1:(p-1),drop=F]

  indices <- sample(nspectra, p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]
  
  pm1 <- 1:(p-1)
  g <- matrix(0, nrow=p, ncol=p-1)
  V <- pm1
  
  replace <- TRUE
  while (replace == TRUE) {
    replace <- FALSE
    
    for (i in 1:p) {
      temp <- 
      
      for (j in 1:nspectra) {
        
      }
    }
  }
}