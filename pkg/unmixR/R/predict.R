.predict <- function(object, newdata=object$data, ...) {

  if (! is.matrix (newdata))
    newdata <- as.matrix (newdata)

  endmembers <- endmembers (object)
  if (! is.matrix (endmembers))
    endmembers <- as.matrix (endmembers)
  endmembers <- t (endmembers)

  t(apply(newdata, 1, function(spectrum) {
    nnls(endmembers, spectrum)$x
  }))
}

##' Predict Endmember Abundances
##'
##' Predicts the abundance percentages of each endmember at all sample points
##' using the Non-Negative Least Squares method
##'
##' @param object The N-FINDR/VCA structure returned by the
##'   \code{\link{nfindr}}/\code{\link{vca}} interface
##' @param newdata If the data stored in the object is not the data that
##'   should be checked for abundances then this parameter allows for passing
##'   in new data
##' @param ... Allow for extra parameters to match the signature of the base
##'   predict function
##' @return A matrix where the abundances for an endmember are returned
##'   column-wise. Each value is in the range \code{0 - 1}
##'
##' @rdname nfindr
##' @export

predict.nfindr <- .predict

##' @rdname vca
##' @export

predict.vca <- .predict
