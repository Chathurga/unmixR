\name{nfindr99}
\alias{nfindr99}
\title{Michael E. Winter's 1999 N-FINDR unmixing algorithm}
\usage{
  nfindr99(data, p, indices, iters = 3 * p)
}
\arguments{
  \item{data}{Data matrix to unmix}

  \item{p}{Number of endmembers}

  \item{indices}{Indices used in the simplex estimation}

  \item{iters}{Max number of iterations, defaults to 3*p}
}
\value{
  The indices that indicate the position of the endmembers
  in the original dataset
}
\description{
  This method should only be called from
  \code{\link{nfindr}}. This technique is based on the fact
  that, in N spectral dimensions, the N-volume contained by
  a simplex formed of the purest pixels is larger than any
  other volume formed from any other combination of pixels.
}
\references{
  Michael E. Winter; "N-FINDR: an algorithm for fast
  autonomous spectral end-member determination in
  hyperspectral data", Proc.  SPIE 3753, Imaging
  Spectrometry V, 266 (October 27, 1999);
  doi:10.1117/12.366289;
}

