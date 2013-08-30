##' Repeat a Vector Column-Wise
##' 
##' Takes in a vector and repeats it column-wise to make a matrix with
##' \code{n} columns
##' 
##' @param v The vector that should be repeated
##' @param n The number of columns in the resulting matrix
##' @return A matrix with \code{n} columns where each column is \code{v}
##' 
##' @rdname repvec.col

.repvec.col <- function(v, n) as.matrix(v)[, rep(1, n)]

.test(.repvec.col) <- function() {
  # test: verify functionality
  
  v <- c(1, 2, 3)
  n <- 3
  
  expected <- matrix(c(
    c(1,1,1),
    c(2,2,2),
    c(3,3,3)
  ), ncol=3, byrow=TRUE)
  
  result <- .repvec.col(v, n)
  
  checkEquals(result, expected)
}