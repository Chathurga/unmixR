nfindr <- function(hyper, p=NULL, iters=NULL) {
  data <- hyper@data$spc
  
  if (is.null(p)) {
    p <- hfc(data, 10^(-5))
  }
  
  if (is.null(iters)) {
    iters <- 3 * p
  }
  
  dims <- dim(data)
  nvariables <- dims[1]
  nsamples <- dims[2]
  
  pca <- prcomp(data)$x[1:(p-1),]
  
  E <- matrix(0, nrow=nvariables, ncol=p)
  C <- matrix(0, nrow=1, ncol=nsamples)
  IDX <- array(0, p)
  
  testMatrix <- matrix(0, nrow=p, ncol=p)
  testMatrix[1,] = 1
  
  for (i in 1:p) {
    rand <- runif(1, min=0, max=1)
    idx <- floor(rand * nsamples) + 1
    
    testMatrix[2:p,i] = pca[idx]
    
    IDX[i] = idx
  }
  
  actualVolume <- abs(det(testMatrix))
  it <- 1;
  v1 <- -1;
  v2 <- actualVolume
  
  while (it <= iters && v2 > v1) {
    for (k in 1:p) {
      for (i in 1:nsamples) {
        actualSample <- testMatrix[2:p,k]
        
        testMatrix[2:p,k] = pca[i]
        volume <- abs(det(testMatrix))
        
        if (volume > actualVolume) {
          actualVolume <- volume
          IDX[k] <- i
        } else {
          testMatrix[2:p,k] = actualSample
        }
      }
    }
    
    it <- it+1
    v1 <- v2
    v2 <- actualVolume
  }
  
  for (i in 1:p) {
    E[,i] = data[,IDX[i]]
    C[IDX[i]] = 1
  }
  
  E
}