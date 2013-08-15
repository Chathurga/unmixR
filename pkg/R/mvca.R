##' Modified Vertex Component Analysis
##' 
##' @references Lopez, S.; Horstrand, P.; Callico, G.M.; Lopez, J.F.;
##' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
##' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
##' Geoscience & Remote Sensing Letters, IEEE, vol.9 no.3 pp.502,506, May 2012
##' doi: 10.1109/LGRS.2011.2172771

mvca <- function(data, p) {
  data <- as.matrix(data)
  Y <- t(svd(data)$u[,1:p])
  Yint <- round2int(Y)
  
  E <- matrix(0, nrow=p, ncol=p+1)
  E[p,1] <- 1
  U <- matrix(0, nrow=p, ncol=p)
  w <- array(1, p)
  proj_acc <- array(0, p)
  indices <- array(0, p)
  
  for (i in 1:p) {
    U[,i] <- E[,i]
    
    if (i >= 3) {
      for (j in 3:p) {
        # ...
      }
    }
    
    proj_w_u <- proj(w, U[,i])
    proj_acc <- proj_acc + proj_w_u
    f <- as.vector(w - proj_acc)
    
    if (i == 1) {
      proj_acc <- array(0, p)
    }
    
    v <- crossprod(round2int(f), Yint)
    
    index <- which.max(v)
    indices[i] <- index
    
    E[,i+1] <- Y[,index]
  }
  
  indices
}

round2int <- function(x) x

proj <- function(x, y) {
  as.vector((crossprod(x, y) / crossprod(y)) %*% y)
}