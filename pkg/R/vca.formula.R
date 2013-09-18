##' @rdname vca
##' @method vca formula
##' @S3method vca formula

vca.formula <- function(formula, frame, p, method="Mod", seed=NULL, ...) {
  data <- model.matrix(formula, frame)
  vca(data, p, method, seed, ...)
}
