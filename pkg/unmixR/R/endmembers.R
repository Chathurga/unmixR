##' Retrieve N-FINDR/VCA Endmembers
##' 
##' Retrieves the endmembers from a dataset using the model returned by
##' \code{\link{nfindr}} or \code{\link{vca}}
##' 
##' @param object The N-FINDR/VCA structure returned by the general
##'   \code{\link{nfindr}}/\code{\link{vca}} interface
##' @param newdata If the data stored in the object does not actually contain
##'   the endmembers then this parameter allows for passing in new data
##' @return A matrix where each row is an endmember as calculated by the
##'   unmixing algorithm

endmembers <- function(object, newdata=object$data) {
  newdata[object$indices, ]
}