##' Retrieve N-FINDR Endmembers
##' 
##' Retrieves the endmembers from a dataset using the model returned by
##' \code{\link{nfindr}}
##' 
##' @param object The N-FINDR structure returned by the general
##'   \code{\link{nfindr}} interface
##' @param newdata If the data stored in the object does not actually contain
##'   the endmembers then this parameter allows for passing in new data
##' @return A matrix where each row is an endmember as calculated by N-FINDR

endmembers <- function(object, newdata=object$data) {
  newdata[object$indices, ]
}