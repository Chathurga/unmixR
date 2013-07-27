\name{nfindrSeqLDU}
\alias{nfindrSeqLDU}
\title{LDU-Sequential N-FINDR: Slightly modified LDU N-FINDR}
\usage{
  nfindrSeqLDU(data, p, simplex, indices, ...)
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
  \code{\link{nfindr}}. While LDU N-FINDR examines a single
  pixel in each endmember position and repeats over all
  pixels, this algorithm considers all pixels in a single
  endmember position and then repeats over all endmember
  positions.
}
\references{
  Dowler, Shaun W.; Takashima, Reymond; Andrews, Mark;
  "Reducing the complexity of the N-FINDR algorithm for
  hyperspectral image analysis.", IEEE Trans Image Process.
  2013 Jul;22(7):2835-48l doi: 10.1109/TIP.2012.2219546
}

