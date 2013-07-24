nfindr.default <- function(data, p, method="LDU", iters=3*p) {
  methods <- c("99", "LDU", "SeqLDU") # valid methods
  
  # check if the method passed in is valid
  if (!method %in% methods) {
    methodsStr <- paste(methods, collapse=", ")
    stop(paste("Invalid option for method parameter, try:", methodsStr))
  }
}
