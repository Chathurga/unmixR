.simplex <- function(data, indices) {
  rbind(rep(1, p), data[indices,])
}