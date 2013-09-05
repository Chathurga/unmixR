nfindrCB <- function(data, p, data, indices, ..., iters = 3) {
  
  no.change <- 0 # number of points that were checked but did not change
  
  # combine the two loops into one
  for (iter in 1 : (iters * p)) {
    i.change <- 1 + iter %% p
    
    i.newcorner <- .maxcorner(data, indices[-i.change], p)
    
    if (i.newcorner == indices [i.change])
      no.change <- no.change + 1
    else
      indices [i.change] <- i.newcorner
    
    # after p points did not change we know that we are converged
    if (nochange > p) break
    }
  }
  
  indices
}

.center <- function (x) {
  if (length(dim(x)) < 2) {
    return(x)
  }
  
  center <- colMeans(x)
  x - rep(center, each=nrow(x))
}

.maxcorner <- function (points, i.simplexbase, p) {
  rot <- svd(.center(points[i.simplexbase,]))$v
  h <- points %*% rot [, p - 1] # we don't need the other dimensions
  h.base <- h [i.simplexbase[1]]
  
  which.max(abs(h - h.base))
}
