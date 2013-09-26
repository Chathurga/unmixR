##' @name vca
##' @rdname vca
##' @method vca formula
##' @S3method vca formula

vca.formula <- function(formula, frame, p, method="Mod", seed=NULL, ...) {
  
  mt <- terms (formula, data = frame)
  
  ## response term is not allowed
  if (attr(mt, "response") > 0L) stop("VCA models cannot have response")
  
  ## drop intercept silently
  attr(mt, "intercept") <- 0L
  
  data <- model.matrix (mt, frame)
  
  vca(data, p, method, seed, ...)
}

.test (vca.formula) <- function (){
  # error on response in formula
  checkException (vca (x ~ ., data.frame (x = matrix (1:4, 2)), p = 2))
  
  # test: check the formula interface
  checkEquals (vca(~ x, .testdata, p = 3)$indices, .correct)
  
  ## check conversion of classes 
  if (require ("hyperSpec")) {
    checkEquals (vca(~ spc, laser, p = 2)$indices, c (4, 81))
  }
    
}