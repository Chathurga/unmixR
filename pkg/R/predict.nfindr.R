##' Predict Endmember Abundances
##' 
##' Predicts the abundance percentages of each endmember at all sample points
##' using the Non-Negative Least Squares method
##' 
##' @param object The N-FINDR structure returned by the general
##'   \code{\link{nfindr}} interface
##' @param newdata If the data stored in the object is not the data that
##'   should be checked for abundances then this parameter allows for passing
##'   in new data
##' @return A matrix where the abundances for an endmember are returned
##'   column-wise. Each value is in the range \code{0 - 1}
##' @rdname predict.nfindr
##' @method predict nfindr
##' @S3method predict nfindr
##' @export

predict.nfindr <- function(object, newdata=object$data, ...) {
  endmembers <- t(newdata[object$indices,])
  
  t(apply(newdata, 1, function(spectrum) {
    nnls(endmembers, spectrum)$x
  }))
}