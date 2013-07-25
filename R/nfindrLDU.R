nfindrLDU <- function(data, p, simplex, indices, ...) {
  nspectra <- nrow(data)
  pm1 <- 1:(p-1) # create a range from 1 to p minus 1
  
  update <- function(simplex, p) {
    g <- matrix(0, nrow=p, ncol=p-1)
    V <- pm1
  
    for (i in 1:p) {
      dup <- simplex
      # swap the i-th and p-th columns of the simplex
      swaps <- 1:p
      swaps[p] = i
      swaps[i] = p
      dup <- dup[,swaps]  #CB: dup [, c (p, i)] <- dup [, c (i, p)]
      
      # get the partitioned components of the simplex matrix
      A <- dup[pm1,pm1]
      b <- dup[pm1,p]
      c <- dup[p,pm1]
      d <- dup[p,p]
      
      g[i,] <- crossprod(c, solve(A))
      V[i] <- as.numeric(abs(d - crossprod(g[i,], b)))
    }
    
    list(simplex=simplex, V=V, g=g)
  }
  
  vars <- update(simplex, p)
  simplex <- vars$simplex
  g <- vars$g
  V <- vars$V
  
  replace <- TRUE
  while (replace == TRUE) {
    replace <- FALSE
    
    for (j in 1:nspectra) {
      y <- c(1, data[j,])
      bj <- y[1:(p-1)]
      dj <- y[p]
      
      for (i in 1:p) {
        Vtest <- as.numeric(abs(dj - crossprod(g[i,], bj)))
        
        if (Vtest > V[i] && !(j %in% indices)) {
          replace <- TRUE
          simplex[,i] <- y
          indices[i] <- j
          
          vars <- update(simplex, p)
          simplex <- vars$simplex
          g <- vars$g
          V <- vars$V
        }
      }
    }
  }
  
  indices
}