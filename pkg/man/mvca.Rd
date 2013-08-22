\name{mvca}
\alias{mvca}
\title{Modified Vertex Component Analysis}
\usage{
  mvca(data, p)
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
  Modified VCA algorithm that aims to reduced the
  algorithmic complexity of the original.
}
\references{
  Lopez, S.; Horstrand, P.; Callico, G.M.; Lopez, J.F.;
  Sarmiento, R., "A Low-Computational-Complexity Algorithm
  for Hyperspectral Endmember Extraction: Modified Vertex
  Component Analysis," Geoscience & Remote Sensing Letters,
  IEEE, vol.9 no.3 pp.502,506, May 2012 doi:
  10.1109/LGRS.2011.2172771
}

