endmembers <- function(model, newdata=model$data) {
  newdata[model$indices, ]
}