\name{nfindrLDU}
\alias{nfindrLDU}
\title{LDU N-FINDR: 2013 Improved N-FINDR algorithm based on LU decompositions}
\usage{
  nfindrLDU(data, p, simplex, indices, ...)
}
\arguments{
  \item{data}{Data matrix to unmix}

  \item{p}{Number of endmembers}

  \item{simplex}{The initial simplex estimation}

  \item{indices}{Indices used in the simplex estimation}
}
\value{
  The indices that indicate the position of the endmembers
  in the original dataset
}
\description{
  This method should only be called from
  \code{\link{nfindr}}. This approach calculates M LU
  decompositions, one with each column permuted to the last
  position and reuses those decompositions on each pixel
  until a permanent replacement requires the calculation of
  a new set of decompositions
}
\references{
  Dowler, Shaun W.; Takashima, Reymond; Andrews, Mark;
  "Reducing the complexity of the N-FINDR algorithm for
  hyperspectral image analysis.", IEEE Trans Image Process.
  2013 Jul;22(7):2835-48l doi: 10.1109/TIP.2012.2219546
}

