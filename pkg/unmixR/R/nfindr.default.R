##' @name nfindr
##' @rdname nfindr
##' @include nfindr.R
##' @export


nfindr.default <- function(data, p,
                           method="LDU", indices=sample(nrow(data), p), ...,
                           drop=FALSE) {

  ## get the selected nfindr method
  nfindrFunc <- get0 (paste0 ("nfindr", method), mode = "function")

  # check if the method passed in was found
  if (is.null (nfindrFunc)) {
    stop ('Invalid option for method parameter, try: "99", "LDU", "SeqLDU", "Brute"')
  }

  ## check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }


  # keep original data
  orig <- data

  # transform the input into a matrix
  data <- as.matrix (data)

  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  if (ncol(data) != p - 1) {
    data <- prcomp(data)$x[, 1:(p-1), drop=FALSE]
  }

  # call the function to get the indices of the endmembers
  indices <- nfindrFunc(data, p, indices, ...)
  # sort the indices to normalise the order between runs
  indices <- sort (indices)

  # return a model
  structure(list(
    data = if (!drop) orig else orig[indices,],
    indices = if (!drop) indices else 1:p
  ), class = "nfindr")
}
