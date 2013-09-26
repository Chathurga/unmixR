##' General interface to Vertex Component Analysis spectral unmixing
##' implementations
##' 
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##' @param formula Formula object
##' @param frame Data frame containing the hyperspectral data
##' @param p Number of endmembers
##' @param method The N-FINDR algorithm to use. Options:
##'   \itemize{
##'     \item 05 (\code{\link{vca05}})
##'     \item Lopez (\code{\link{vcaLopez}})
##'     \item Mod (\code{\link{vcaMod}})
##'   }
##'   Default: Mod as it is the most efficient
##' @param seed vca05 and vcaLopez both need to generate a random vector. Set
##'   the random number generator seed with this
##' @param ... Extra parameters that will get passed into selected method, see
##'   selected method for options
##' 
##' @return A structure which contains:
##'   \itemize{
##'     \item \strong{data}: the original data
##'     \item \strong{indices}: the indices of the calculated endmembers
##'   }
##' 
##' @rdname vca
##' @export

vca <- function(...) {
  UseMethod("vca")
}

.test(vca) <- function() {
  data <- laser
  seed <- 10 # fix the seed
  
  # test: vca produces error for invalid values of p
  
  checkException(nfindr(data, p="---"))
  checkException(nfindr(data, p=0))
  
  # test: vca produces error for invalid method
  
  checkException(nfindr(data, p, method="invalid"))
  
  # test: vca05 and vcaLopez output the correct answers
  
  ans05 <- vca(data, 2, method="05", seed=seed)
  checkEquals(ans05$indices, c(4, 84))
  
  ansLopez <- vca(data, 2, method="Lopez", seed=seed)
  checkEquals(ansLopez$indices, c(4, 81))
  
  # test vcaLopez and vcaMod should output the same answer
  
  ansMod <- vca(data, 2, method="Mod", seed=seed)
  checkEquals(ansLopez$indices, ansMod$indices)
  
  # test: check the formula interface
  output.formula <- vca(~ 0 + ., as.data.frame(data), p = 2)$indices
  checkEquals(output.formula, ansMod$indices)

  # test: if hyperSpec is available, test on hyperSpec object
  # tests also the correct application of as.matrix.
  if (require ("hyperSpec"))
    checkEquals (vca(~ spc, laser, p = 2)$indices, c (4, 81))
  
}
