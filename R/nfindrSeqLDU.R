nfindrSeqLDU <- function(data, p) {
  data <- as.matrix(data)
  nspectra <- dim(data)[1]
  
  reduced <- prcomp(data)$x[,1:(p-1),drop=F]

  indices <- sample(nspectra, p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]
  
  pm1 <- 1:(p-1) # create a range from 1 to p minus 1
  g <- matrix(0, nrow=p, ncol=p-1)
  V <- pm1
  
  replace <- TRUE
  while (replace == TRUE) {
    replace <- FALSE
    
    for (i in 1:p) {
      dup <- simplex # make a copy of the simplex
      # swap the i-th and p-th columns of the simplex
      temp <- dup[,p]
      dup[,p] = dup[,i]
      dup[,i] = temp
      
      # get the partitioned components of the simplex matrix
      A <- dup[pm1,pm1]
      b <- dup[pm1,p]
      c <- dup[p,pm1]
      d <- dup[p,p]
      
      g[i,] <- crossprod(c, solve(A))
      V[i] <- as.numeric(abs(d - crossprod(g[i,], b)))
      
      for (j in 1:nspectra) {
        y <- c(1, reduced[j,])
        bj <- y[1:(p-1)]
        dj <- y[p]
        
        Vtest <- as.numeric(abs(dj - crossprod(g[i,], bj)))
        
        if (Vtest > V[i]) {
          replace <- TRUE
          simplex[,i] <- y
          indices[i] <- j
        }
      }
    }
  }
  
  indices
}