lvca <- function(data, p) {
  data <- as.matrix(data)
  Y <- t(prcomp(data)$x[,1:p])
  
  E <- matrix(0, nrow=p, ncol=p)
  E[p,1] <- 1
  I <- diag(p)
  
  indices <- array(0, p)
  
  for (i in 1:p) {
    w <- runif(p)
    x <- (I - (E %*% ginv(E))) %*% w
    f <- x / sqrt(sum(x^2))
    v <- crossprod(f, Y)
    
    index <- which.max(v)
    indices[i] <- index
    E[, i] <- Y[, index]
  }
  
  indices
}