##' LDU N-FINDR: 2013 Improved N-FINDR algorithm based on LU decompositions
##' 
##' This method should only be called from \code{\link{nfindr}}.
##' This approach calculates M LU decompositions, one with each column
##' permuted to the last position and reuses those decompositions on each
##' pixel until a permanent replacement requires the calculation of a new set
##' of decompositions
##' 
##' @param data Data matrix to unmix
##' @param p Number of endmembers
##' @param indices Indices used in the simplex estimation
##' @return The indices that indicate the position of the endmembers in the
##'   original dataset
##' 
##' @references  Dowler, Shaun W.; Takashima, Reymond; Andrews, Mark;
##'   "Reducing the complexity of the N-FINDR algorithm for hyperspectral
##'   image analysis.", IEEE Trans Image Process. 2013 Jul;22(7):2835-48l
##'   doi: 10.1109/TIP.2012.2219546

nfindrLDU <- function(data, p, indices, ...) {
  simplex <- .simplex(data, indices)
  nspectra <- nrow(data)
  pm1 <- 1:(p-1) # create a range from 1 to p minus 1
  
  update <- function(simplex, p) {
    g <- matrix(0, nrow=p, ncol=p-1)
    V <- pm1
  
    for (i in 1:p) {
      dup <- simplex
      # swap the i-th and p-th columns of the simplex
      dup [, c(p, i)] <- dup [, c(i, p)]
      
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