##' Predict Endmember Abundances
##' 
##' Predicts the abundance precentages of each endmember at all sample points
##' using the Non-Negative Least Squares method
##' 
##' @param object The N-FINDR structure returned by the general
##'   \code{\link{nfindr}} interface
##' @return A matrix where the abundances for an endmember are returned
##'   column-wise. Each value is in the range \code{0 - 1}
##' @rdname predict.nfindr
##' @method predict nfindr
##' @S3method predict nfindr
##' @export

predict.nfindr <- function(object, ...) {
  if (is.null(object$data)) {
    stop("Orginal data was dropped, needs to be reassigned to object$data")
  }
  
  data <- object$data
  endmembers <- t(data[object$indices,])
  
  t(apply(data, 1, function (spectrum) {
    nnls(endmembers, spectrum)$x
  }))
}