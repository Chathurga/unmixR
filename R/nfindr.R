##' General interface to N-FINDR spectral unmixing implementations
##' 
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##' 
##' @param data Data to unmix. Will be converted to a matrix using as.matrix.
##'   The matrix should contain a spectrum per row.
##' @param p Number of endmembers
##' @param method The N-FINDR algorithm to use. Options:
##'   \itemize{
##'     \item 99 (\code{\link{nfindr99}})
##'     \item LDU (\code{\link{nfindrLDU}})
##'     \item SeqLDU (\code{\link{nfindrSeqLDU}})
##'   }
##'   Default: LDU as it generally performs the best
##' @param iters Only applies when using nfindr99, defines the max number of
##'   iterations before the algorithm halts. This is only
##'   hit in cases where the correct number of endmembers could not be found.
##'   Default: 3*p
##' @param drop Boolean that indicates whether the \code{data} parameter
##'   should be stored in the resulting structure. This should only be set to
##'   \code{True} when \code{data} was passed in already reduced
##' 
##' @return A structure which contains:
##'   \itemize{
##'     \item \strong{data}: the original data (or NULL if drop is set to
##'                          \code{True})
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers
##'   }

nfindr <- function(data, p, method, iters, drop, ...) UseMethod("nfindr")

nfindr.default <- function(data, p, method="LDU", iters=3*p, drop=FALSE) {
  methods <- c("99", "LDU", "SeqLDU") # valid methods
  
  # check if the method passed in is valid
  if (!method %in% methods) {
    methodsStr <- paste(methods, collapse=", ")
    stop(paste("Invalid option for method parameter, try:", methodsStr))
  }
  
  # transform the input into a matrix
  data <- as.matrix(data)
  
  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  orig <- data
  if (ncol(data) != p - 1) {
    data <- prcomp(data)$x[,1:(p-1),drop=F]
  }
  
  # select random indices that form the initial simplex
  # this simplex will be inflated until the pure pixels are found
  indices <- sample(nrow(data), p)
  simplex <- rbind(rep(1, p), data[indices,])
  
  # get the selected nfindr method
  nfindrFunc <- get(paste("nfindr", method, sep=""))
  # call the function to get the indices of the endmembers
  indices <- nfindrFunc(data, p, simplex, indices, iters)
  
  # return a model
  structure(list(
    data = if (!drop) orig else NULL,
    indices = indices
  ), class = "nfindr")
}
