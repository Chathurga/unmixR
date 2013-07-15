nfindrLDU <- function(data, p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  reduced <- prcomp(data)$x[,1:(p-1),drop=F]

  indices <- sample(nrow(reduced), p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]

  pm1 <- 1:p-1 # create a range from 1 to p minus 1
  
  g <- matrix(0, nrow=p, ncol=p-1)
  V <- pm1

  for (i in 1:p) {
    # swap the i-th and p-th columns of the simplex
    dup <- simplex
    swaps <- 1:p
    swaps[p] = i
    swaps[i] = p
    dup <- dup[,swaps]
    
    # get the partitioned components of the simplex matrix
    A <- dup[pm1,pm1]
    b <- dup[pm1,p]
    c <- dup[p,pm1]
    d <- dup[p,p]
    
    g[i,] <- t(c) %*% solve(A)
    V[i] <- abs(d - (t(g[i,]) %*% b))
  }
  
  g
}