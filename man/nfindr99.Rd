\name{nfindr99}
\alias{nfindr99}
\title{Michael E. Winter's 1999 N-FINDR unmixing algorithm}
\usage{
  nfindr99(data, p, iters = 3 * p)
}
\arguments{
  \item{data}{Data to unmix. Will be converted to a matrix
  using as.matrix.  The matrix should contain a spectrum
  per row.}

  \item{p}{Number of endmembers (will be estimated using
  \code{\link{hfc}} if omitted)}

  \item{iters}{Max number of iterations (defaults to 3 *
  p)}
}
\value{
  A structure which contains: \itemize{ \item
  \strong{data}: the original data (or NULL if the input
  data was already reduced) \item \strong{indexes}: the
  indexes of the spectra which increased the simplex volume
  the most. These are the indexes of the endmembers }
}
\description{
  This technique is based on the fact that in N spectral
  dimensions, the N-volume contained by a simplex formed of
  the purest pixels is larger than any other volume formed
  from any other combination of pixels.
}
\references{
  Michael E. Winter; "N-FINDR: an algorithm for fast
  autonomous spectral end-member determination in
  hyperspectral data", Proc.  SPIE 3753, Imaging
  Spectrometry V, 266 (October 27, 1999);
  doi:10.1117/12.366289;
}

