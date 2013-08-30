\name{vca}
\alias{vca}
\title{General interface to Vertex Component Analysis spectral unmixing
implementations}
\usage{
  vca(data, p, method, seed)
}
\arguments{
  \item{data}{Data to unmix. It will be converted to a
  matrix using as.matrix. The matrix should contain a
  spectrum per row.}

  \item{p}{Number of endmembers}

  \item{method}{The N-FINDR algorithm to use. Options:
  \itemize{ \item 05 (\code{\link{vca05}}) \item Lopez
  (\code{\link{vcaLopez}}) \item Mod (\code{\link{vcaMod}})
  } Default: Mod as it is the most efficient}

  \item{seed}{vca05 and vcaLopez both need to generate a
  random vector. Set the random number generator seed with
  this}
}
\value{
  A structure which contains: \itemize{ \item
  \strong{data}: the original data \item \strong{indices}:
  the indices of the calculated endmembers }
}
\description{
  This algorithm is based on the geometry of convex sets.
  It exploits the fact that endmembers occupy the vertices
  of a simplex.
}

