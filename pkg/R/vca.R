##' General interface to Vertex Component Analysis spectral unmixing
##' implementations
##' 
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
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

vca <- function(data, p, method, seed, ...) {
  UseMethod("vca")
}

vca.default <- function(data, p, method="Mod", seed=NULL, ...) {
  methods <- c("05", "Lopez", "Mod")
  
  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer greater than 2")
  }
  
  # check if the method passed in is valid
  if (!method %in% methods) {
    methodsStr <- paste(methods, collapse=", ")
    stop(paste("Invalid option for method parameter, try:", methodsStr))
  }
  
  # transform the input into a matrix
  data <- as.matrix(data)
  
  # set the random number generator seed if supplied
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  vcaFunc <- get(paste("vca", method, sep=""))
  
  indices <- sort(vcaFunc(data, p, ...))
  
  structure(list(
    data = data,
    indices = indices
  ), class = "vca")
}

.test(nfindr) <- function() {
  seed <- 10 # fix the seed
  
  # test: vca05 and vcaLopez output the correct answers
  
  ans1 <- vca(laser, 2, method="05", seed=seed)
  checkEquals(ans1$indices, c(4, 84))
  
  ans2 <- vca(laser, 2, method="Lopez", seed=seed)
  checkEquals(ans2$indices, c(4, 81))
}