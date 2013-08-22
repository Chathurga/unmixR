vca <- function(data, p, method, seed) {
  UseMethod("vca")
}

vca.default <- function(data, p, method="Mod", seed=NULL) {
  methods <- c("05", "Lopez", "Mod")
}