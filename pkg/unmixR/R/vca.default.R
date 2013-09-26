##' @name vca
##' @rdname vca
##' @method vca default
##' @S3method vca default

vca.default <- function(data, p, method= c("Mod", "Lopez", "05"), seed=NULL, ...) {

  # check if the method passed in is valid
  method <- match.arg (method)

  # transform the input into a matrix
  data <- as.matrix (data)
  
  # check for p being with the valid range, >= 2
  if (!is.numeric (p) || p < 2 || p > ncol (data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }
  
  # set the random number generator seed if supplied
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  vcaFunc <- get(paste("vca", method, sep=""))
  val <- vcaFunc(data, p, ...)
  
  if (method == "05") {
    struct <- val
  } else {
    struct <- list(
      data = data,
      indices = sort(val)
    )
  }
  
  
  structure(struct, class = "vca")
}


.test(vca.default) <- function() {
  # test: vca produces error for invalid values of p
  checkException (vca (.testdata$x, p = "---"))
  checkException (vca (.testdata$x, p = 0))
  checkException (vca (.testdata$x, p = 1))
  checkException (vca (.testdata$x, p = 4))
  
  # test: vca produces error for invalid method
  checkException(vca (.testdata$x, p, method="invalid"))
  
  ## test correct calculations for the available methods
  methods <- eval (formals (vca.default)$method)
  
  for (m in methods) {
    ## .testdata has 3 components, so picking 2 out of 3
    checkTrue (all (vca (.testdata$x, p = 2, method = m)$indices %in% .correct), 
               msg = sprintf ("%s: .testdata, p = 2", m))
    
    ## all 3 components should be recovered, vca output is sorted. 
    checkEquals (vca (.testdata$x, p = 3, method = m)$indices, .correct, 
                 msg = sprintf ("%s: .testdata, p = 3", m))
  }
  
  # test: if hyperSpec is available, test on hyperSpec object
  # tests also the correct application of as.matrix.
  if (require ("hyperSpec")) {
    checkEquals (vca (laser, p = 2)$indices, c (4, 81))
  }
}
