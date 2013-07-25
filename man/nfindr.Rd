\name{nfindr}
\alias{nfindr}
\title{General interface to N-FINDR spectral unmixing implementations}
\usage{
  nfindr(data, p, method, iters, drop, ...)
}
\arguments{
  \item{data}{Data to unmix. Will be converted to a matrix
  using as.matrix.  The matrix should contain a spectrum
  per row.}

  \item{p}{Number of endmembers}

  \item{method}{The N-FINDR algorithm to use. Options:
  \itemize{ \item 99 (\code{\link{nfindr99}}) \item LDU
  (\code{\link{nfindrLDU}}) \item SeqLDU
  (\code{\link{nfindrSeqLDU}}) } Default: LDU as it
  generally performs the best}

  \item{iters}{Only applies when using nfindr99, defines
  the max number of iterations before the algorithm halts.
  This is only hit in cases where the correct number of
  endmembers could not be found.  Default: 3*p}

  \item{drop}{Boolean that indicates whether the
  \code{data} parameter should be stored in the resulting
  structure. This should only be set to \code{True} when
  \code{data} was passed in already reduced}
}
\value{
  A structure which contains: \itemize{ \item
  \strong{data}: the original data (or NULL if drop is set
  to \code{True}) \item \strong{indices}: the indices of
  the spectra which increased the simplex volume the most.
  These are the indices of the endmembers }
}
\description{
  All the N-FINDR techniques are based on the fact that, in
  N spectral dimensions, the N-volume contained by a
  simplex formed of the purest pixels is larger than any
  other volume formed from any other combination of pixels.
}

