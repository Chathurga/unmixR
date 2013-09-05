center <- function (x, center = TRUE) {
  if (length(dim(x)) < 2) {
    return(as.matrix(x))
  }
  
  if (isTRUE(center)) {
    center <- colMeans(x)
  }
  
  x - rep (center, each = nrow(x))
}

maxcorner <- function (points, i.simplexbase, p) {
  rot <- svd (center (points [i.simplexbase,]))$v
  points <- points %*% rot
  z.base <- points [i.simplexbase [1], p - 1]
  which.max (abs (points [, p - 1] - z.base))
}

nfindrCB <- function(data, p, ...) {
  i.simplex <- sample(nrow(data), p)
  
  old.simplex <- i.simplex
  
  # combine the two loops into one
  for (iter in 1 : (3 * p)) {
    i.change <- 1 + iter %% p
    i.simplex[i.change] <- maxcorner (data, i.simplex [- i.change], p)
    
    # all points optimized at least once
    # from now on if the point didn't change then we're at the solution
    if (iter > p) {
      if (all(i.simplex == old.simplex))
        break
      else
        old.simplex <- i.simplex
    }
  }
  
  i.simplex
}
