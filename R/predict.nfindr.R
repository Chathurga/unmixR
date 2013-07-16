##' ---
##'
##' @param struct The nfindr structure returned by one of the nfindr
##'   algorithms
##' @rdname predict
##' @method predict nfindr
##' @S3method predict nfindr
##' @export

predict.nfindr <- function(struct) {
  data <- struct$data
  endmembers <- t(struct$data[struct$indices,])
  
  apply(data, 1, function(spectrum) {
    nnls(endmembers, spectrum)$x
  })
}