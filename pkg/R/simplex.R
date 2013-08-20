.simplex <- function(data, p, indices) {
  rbind(rep(1, p), data[indices,])
}