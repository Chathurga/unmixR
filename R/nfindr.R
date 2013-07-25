nfindr.default <- function(data, p, method="LDU", iters=3*p, drop=FALSE) {
  methods <- c("99", "LDU", "SeqLDU") # valid methods
  
  # check if the method passed in is valid
  if (!method %in% methods) {
    methodsStr <- paste(methods, collapse=", ")
    stop(paste("Invalid option for method parameter, try:", methodsStr))
  }
  
  # transform the input into a matrix
  data <- as.matrix(data)
  
  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  if (ncol(data) != p - 1) {
    data <- prcomp(data)$x[,1:(p-1),drop=F]
  }
  
  # select random indices that form the initial simplex
  # this simplex will be inflated until the pure pixels are found
  indices <- sample(nrow(data), p)
  simplex <- matrix(0, nrow=p, ncol=p)
  simplex[1,] <- 1
  simplex[2:p,] <- reduced[indices,]
  
  # get the selected nfindr method
  nfindrFunc <- get(paste("nfindr", method, sep=""))
  # call the function to get the indices of the endmembers
  indices <- nfindrFunc(data, p, iters=iters)
  
  # return a model
  structure(list(
    data = if (!drop) data else NULL,
    indices = indices
  ), class = "nfindr")
}
