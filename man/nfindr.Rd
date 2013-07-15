\name{nfindr}
\alias{nfindr}
\title{N-FINDR unmixing algorithm}
\usage{
  nfindr(data, p = hfc(data, 10^(-5)), iters = 3 * p)
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
  Returns a (p x no. of spectra) matrix where each row is a
  pure component
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

