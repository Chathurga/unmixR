\name{vca05}
\alias{vca05}
\title{Vertex Component Analysis unmixing algorithm}
\usage{
  vca05(data, p)
}
\arguments{
  \item{data}{Data to unmix. It will be converted to a
  matrix using as.matrix. The matrix should contain a
  spectrum per row.}

  \item{p}{Number of endmembers}
}
\value{
  Endmember indices
}
\description{
  This algorithm is based on the geometry of convex sets.
  It exploits the fact that endmembers occupy the vertices
  of a simplex.
}
\references{
  Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
  analysis: a fast algorithm to unmix hyperspectral data,"
  Geoscience and Remote Sensing, vol.43, no.4, pp.898,910,
  April 2005; doi: 10.1109/TGRS.2005.844293
}

