\name{nfindr}
\alias{nfindr}
\title{N-FINDR (Michael E. Winter, 1999) unmixing algorithm}
\usage{
  nfindr(data, p = hfc(data, 10^(-5)), iters = 3 * p)
}
\arguments{
  \item{data}{Spectra matrix to unmix}

  \item{p}{Number of endmembers (will be estimated using
  \code{\link{hfc}} if omitted)}

  \item{iters}{Max number of iterations (defaults to 3 *
  p)}
}
\value{
  (-return-)
}
\description{
  (-description-)
}

