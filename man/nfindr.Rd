\name{nfindr}
\alias{nfindr}
\title{N-FINDR unmixing algorithm}
\usage{
  nfindr(data, p = hfc(data, 10^(-5)), iters = 3 * p)
}
\arguments{
  \item{data}{Spectra matrix to unmix. Each column of the
  matrix should represent a spectrum}

  \item{p}{Number of endmembers (will be estimated using
  \code{\link{hfc}} if omitted)}

  \item{iters}{Max number of iterations (defaults to 3 *
  p)}
}
\value{
  (-return-)
}
\description{
  This technique is based on the fact that in N spectral
  dimensions, the N-volume contained by a simplex formed of
  the purest pixels is larger than any other volume formed
  from any other combination of pixels.
}

