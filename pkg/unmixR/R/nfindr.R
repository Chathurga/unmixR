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
##' @param formula Formula object
##' @param frame Data frame containing the hyperspectral data
##' @param p Number of endmembers
##' @param method The N-FINDR algorithm to use. Options:
##'   \itemize{
##'     \item 99 (\code{\link{nfindr99}})
##'     \item LDU (\code{\link{nfindrLDU}})
##'     \item SeqLDU (\code{\link{nfindrSeqLDU}})
##'     \item Brute (\code{\link{nfindrBrute}})
##'   }
##'   Default: LDU as it generally performs the best
##' @param indices Locations of the rows in the dataset that will be used to
##'   form the initial simplex. Default: Randomly selected indices
##' @param ... Extra parameters that will get passed into selected method, see
##'   selected method for options
##' @param drop Boolean that indicates whether the \code{data} parameter
##'   should be stored in the resulting structure. This should only be set to
##'   \code{True} when \code{data} was passed in already reduced
##'
##' @return A structure which contains:
##'   \itemize{
##'     \item \strong{data}: the original data or reduced data if drop is
##'                          set to \code{True}
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers. If drop is set to
##'                             \code{True} then indices will be 1 to p
##'   }
##'
##' @rdname nfindr
##' @export
##' @include unmixR-package.R

nfindr <- function(...) {
  UseMethod("nfindr")
}

.test(nfindr) <- function() {
  data <- as.matrix(laser)
  p <- 2
  indices <- c(1, 2)

  # test: nfindr produces error for invalid values of p

  checkException(nfindr(data, p="---"))
  checkException(nfindr(data, p=0))

  # test: nfindr produces error for invalid method

  checkException(nfindr(data, p, method="invalid"))

  # test: nfindr default produces the correct answer

  output <- nfindr(data, p)$indices
  checkTrue(output == c(4, 79))

  # test: all N-FINDR methods produce the same output

  methods <- c("99", "LDU", "SeqLDU", "CB", "Brute")

  outputs <- sapply(1:4, function(i) {
    nfindr(data, p, methods[i])$indices
  })

  checkTrue(all(outputs[,1] == outputs))

  # test: check the formula interface

  output.formula <- nfindr(~ 0 + ., as.data.frame(data), p)$indices
  checkEquals(output.formula, output)
}
