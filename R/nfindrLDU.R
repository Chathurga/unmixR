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
    simplex[p] = i
    simplex[i] = p
    simplex <- simplex[,swaps]
    
    # get the partitioned components of the simplex matrix
    A <- simplex[pm1,pm1]
    b <- simplex[pm1,p]
    c <- simplex[p,pm1]
    d <- simplex[p,p]
    
    g[i,] <- t(c) %*% solve(A)
    V[i] <- abs(d - (t(g[i,]) %*% b))
  }
  
  replace <- T
  while (replace == T) {
    replace <- F
    
    for (j in 1:nspectra) {
      y <- reduced[j,]
      bj <- c(1, y[1:p-2])
      dj <- y[p-1]
      
      for (i in 1:p) {
        Vtest <- abs(dj - (t(g[i,]) %*% bj))
        
        if (Vtest > V[i]) {
          replace <- T
          V[i] <- Vtest
          simplex[i,] = y
        }
      }
    }
  }
  
  g
}