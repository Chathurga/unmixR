##' @param struct The nfindr structure returned by one of the nfindr
##'   algorithms
##' @rdname predict #CB: suggests: @rdname nfindr
##' @method predict nfindr
##' @S3method predict nfindr
##' @export

predict.nfindr <- function(object, ...) {  #CB: obey signature of generic, see ?predict
  data <- object$data
  endmembers <- t(object$data [object$indices,])
  
  apply(data, 1, function(spectrum) {
    nnls(endmembers, spectrum)$x
  })
}