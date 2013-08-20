##' General interface to N-FINDR spectral unmixing implementations
##' 
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row. This data will
##'   be dimensionally reduced using PCA. If you want to reduce the data using
##'   some other method then reduce it first and set drop to \code{TRUE}
##' @param p Number of endmembers
##' @param method The N-FINDR algorithm to use. Options:
##'   \itemize{
##'     \item 99 (\code{\link{nfindr99}})
##'     \item LDU (\code{\link{nfindrLDU}})
##'     \item SeqLDU (\code{\link{nfindrSeqLDU}})
##'     \item Brute (\code{\link{nfindrBrute}})
##'   }
##'   Default: LDU as it generally performs the best
##' @param ... Extra parameters that will get passed into selected method, see
##'   selected method for options
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

nfindr <- function(data, p, method, indices, ..., drop) {
  UseMethod("nfindr")
}

nfindr.default <- function(data, p,
                           method="LDU", indices=sample(nrow(data), p), ...,
                           drop=FALSE) {
  methods <- c("99", "LDU", "SeqLDU", "Brute") # valid methods
  
  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer greater than 2")
  }
  
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
    data <- prcomp(data)$x[, 1:(p-1), drop=FALSE]
  }
  
  # select random indices that form the initial simplex
  # this simplex will be inflated until the pure pixels are found
  indices <- sample(nrow(data), p)
  simplex <- rbind(rep(1, p), data[indices,])
  
  # get the selected nfindr method
  nfindrFunc <- get(paste("nfindr", method, sep=""))
  # call the function to get the indices of the endmembers
  indices <- nfindrFunc(data, p, indices, ...)
  # sort the indices to normalise the order between runs
  indices <- sort(indices)
  
  # return a model
  structure(list(
    data = if (!drop) orig else orig[indices,],
    indices = if (!drop) indices else 1:p
  ), class = "nfindr")
}

.test(nfindr) <- function() {
  p <- 2
  indices <- c(1, 2)
  
  # test: nfindr produces an error for invalid values of p
  
  checkException(nfindr(laser, p="---"))
  checkException(nfindr(laser, p=0))
  
  # test: all N-FINDR methods produce the same output
  
  methods <- c("99", "LDU", "SeqLDU", "Brute")
  
  outputs <- sapply(1:4, function(i) {
    nfindr(laser, p, methods[i])$indices
  })
  
  checkTrue(all(outputs[,1] == outputs))
}
