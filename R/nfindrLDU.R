nfindrLDU <- function(data, p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  reduced <- prcomp(data)$x[,1:(p-1),drop=F]

  indices <- sample(nrow(reduced), p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]

  pm1 <- 1:p-1 # create a range from 1 to p minus 1
  
  g <- array(0, p)
  V <- array(0, p)

  for (i in 1:p) {
    idx <- 1:p
    idx[p] = i
    idx[i] = p
    
    simplex <- simplex[,idx]
    
    # get the partitioned components of the simplex matrix
    A <- simplex[pm1,pm1]
    b <- simplex[pm1,p]
    c <- simplex[p,pm1]
    d <- simplex[p,p]
    
    g[i] <- t(c) * solve(A)
    V[i] <- abs(d - (t(g[i]) * b))
  }
  
  V
}