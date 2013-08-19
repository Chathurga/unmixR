nfindrBrute <- function(data, p) {
  data <- as.matrix(data)
  data <- prcomp(data)$x[, 1:(p-1), drop=FALSE]
  
  # generate all possible unique combinations of p indexes
  combos <- combn(nrow(data), p, simplify=TRUE)
  n <- ncol(combos)
  # generate the volumes of all the simplexes using the indexes
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]
    simplex <- rbind(rep(1, p), data[idx,])
    
    abs(det(simplex))
  })
  
  col <- which.max(volumes)
  combos[, col]
}