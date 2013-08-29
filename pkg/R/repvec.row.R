##' Repeat a Vector Row-Wise
##' 
##' Takes in a vector and repeats it column-wise to make a matrix with
##' \code{n} rows
##' 
##' @param v The vector that should be repeated
##' @param n The number of rows in the resulting matrix
##' @return A matrix with \code{n} rows where each row is \code{v}
##' 
##' @rdname repvec.row

.repvec.row <- function(v, n) t(as.matrix(v))[rep(1, n), ]