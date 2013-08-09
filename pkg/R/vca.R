##' Vertex Component Analysis unmixing algorithm
##' 
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##' @param p Number of endmembers
##' @return Endmember indices
##' 
##' @references Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol.43, no.4, pp.898,910, April 2005;
##'   doi: 10.1109/TGRS.2005.844293

vca <- function(data, p) {
  # get the matrix representation of the input (if it's some other class)
  data <- t(as.matrix(data))
  
  nspectra <- nrow(data)
  nsamples <- ncol(data)
  
  # estimate the signal to noise ratio
  rowMean <- apply(data, 1, mean) # get the mean of each row
  # repeat the column of row means so that it matches the size of the data
  repMean <- repvec.col(rowMean, nsamples)
  zMean <- data - repMean # zero mean the data
  Ud <- svd(tcrossprod(zMean) / nsamples, nv=p)$u[,1:p]
  zProj <- crossprod(Ud, zMean) # project the zero mean data
  
  E <- function(M, n) sum(c(M)^2 / n) # expectation operator
  pr <- E(data, nsamples)
  prp <- E(crossprod(Ud, zMean), nsamples) + crossprod(rowMean)
  SNR <- 10 * log10((prp - (p / nspectra) * pr) / (pr - prp))
  
  # signal to noise threshold
  SNRth <- 15 + 10 * log10(p)
  # if the estimated SNR is over a certain threshold ...
  if (SNR > SNRth) {
    d <- p
    Ud <- svd(tcrossprod(data) / nsamples)$u[,1:d]
    
    x <- crossprod(Ud, data)
    u <- apply(x, 1, mean)
    dataProj <- Ud %*% x[1:d,] # project the input matrix
    
    repu <- repvec.col(u, nsamples)
    y <- x / repvec.row(apply(t(x * repu), 1, sum), d)
  } else {
    d <- p - 1
    
    x <- zProj[1:d, ]
    dataProj <- Ud[, 1:d] %*% x + repMean
    c <- max(sum(x^2))^0.5
    y <- rbind(x, c)
  }
  
  indices <- array(0, p)
  A <- matrix(0, nrow=p, ncol=p)
  A[p,1] <- 1
  
  for (i in 1:p) {
    w <- runif(p)
    f <- w - A %*% ginv(A) %*% w;
    f <- f / sqrt(sum(f^2));
    
    v <- abs(crossprod(f, y))
    indices[i] <- which.max(v) # get index of max value
    A[,i] <- y[, indices[i]]
  }
  
  endmembers <- dataProj[, indices]
  
  structure(list(
    endmembers = t(endmembers),
    indices = indices
  ), class = "vca")
}

repvec.col <- function(v, n) as.matrix(v)[, rep(1, n)]
repvec.row <- function(v, n) t(as.matrix(v))[rep(1, n), ]