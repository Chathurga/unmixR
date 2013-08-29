vca <- function(data, p, method, seed) {
  UseMethod("vca")
}

vca.default <- function(data, p, method="Mod", seed=NULL) {
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
  
  indices <- sort(vcaFunc(data, p))
  
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