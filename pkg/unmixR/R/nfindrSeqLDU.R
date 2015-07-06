##' LDU-Sequential N-FINDR: Slightly modified LDU N-FINDR
##'
##' This method should only be called from \code{\link{nfindr}}.
##' While LDU N-FINDR examines a single pixel in each endmember position and
##' repeats over all pixels, this algorithm considers all pixels in a single
##' endmember position and then repeats over all endmember positions.
##'
##' @param data Data matrix to unmix
##' @param p Number of endmembers
##' @param indices Indices used in the simplex estimation
##' @param ... Extra unused parameters that get passed in from
##'   \code{\link{nfindr}}
##' @return The indices that indicate the position of the endmembers in the
##'   original dataset
##' @export
##'
##' @references  Dowler, Shaun W.; Takashima, Reymond; Andrews, Mark;
##'   "Reducing the complexity of the N-FINDR algorithm for hyperspectral
##'   image analysis.", IEEE Trans Image Process. 2013 Jul;22(7):2835-48l
##'   doi: 10.1109/TIP.2012.2219546

nfindrSeqLDU <- function(data, p, indices, ...) {
  simplex <- .simplex(data, p, indices)
  nspectra <- nrow(data)
  pm1 <- 1:(p-1) # create a range from 1 to p minus 1
  g <- matrix(0, nrow=p, ncol=p-1)
  V <- pm1

  replace <- TRUE
  while (replace == TRUE) {
    replace <- FALSE

    for (i in 1:p) {
      dup <- simplex # make a copy of the simplex
      # swap the i-th and p-th columns of the simplex
      dup [, c(p, i)] <- dup [, c(i, p)]

      # get the partitioned components of the simplex matrix
      A <- dup[pm1,pm1]
      b <- dup[pm1,p]
      c <- dup[p,pm1]
      d <- dup[p,p]

      g[i,] <- crossprod(c, solve(A))
      V[i] <- as.numeric(abs(d - crossprod(g[i,], b)))

      for (j in 1:nspectra) {
        y <- c(1, data[j,])
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
