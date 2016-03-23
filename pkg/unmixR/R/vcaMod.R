##' Modified Vertex Component Analysis
##'
##' Modified VCA algorithm that aims to reduced the algorithmic complexity of
##' the original.
##'
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##' @param p Number of endmembers
##' @return Endmember indices
##'
##' @references Lopez, S.; Horstrand, P.; Callico, G.M.; Lopez, J.F.;
##' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
##' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
##' Geoscience & Remote Sensing Letters, IEEE, vol.9 no.3 pp.502,506, May 2012
##' doi: 10.1109/LGRS.2011.2172771
##' @export

vcaMod <- function(data, p) {
  data <- as.matrix(data)
  Y <- t(prcomp(data)$x[,1:p])

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
        # E[,i] and U[,j-1] are the same on 3rd iter, projection produces
        # no change, thus U[,i] - proj_e_u = 0
        proj_e_u <- .proj(E[,i], U[,j-1])
        U[,i] <- U[,i] - proj_e_u
      }
    }

    # error for p >= 3 on iter 3 due to U[,i] being 0 so projection fails
    proj_w_u <- .proj(w, U[,i])
    proj_acc <- proj_acc + proj_w_u
    f <- as.vector(w - proj_acc)

    if (i == 1) {
      proj_acc <- array(0, p)
    }

    v <- crossprod(f, Y)

    index <- which.max(v) # always appears to produce the same index

    if (.options ("debuglevel") >= 1L){
      print (which.max (v))
      print (which.min (v))
    }

    indices[i] <- index

    E[,i+1] <- Y[,index]
  }

  indices
}

.proj <- function(x, y) {
  as.vector((crossprod(x, y) / crossprod(y)) %*% y)
}
