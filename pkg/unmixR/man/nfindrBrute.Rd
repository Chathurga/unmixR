\name{nfindrBrute}
\alias{nfindrBrute}
\title{Brute Force N-FINDR}
\usage{
  nfindrBrute(data, p, ...)
}
\arguments{
  \item{data}{Data matrix to unmix}

  \item{p}{Number of endmembers}

  \item{...}{Extra unused parameters that get passed in
  from \code{\link{nfindr}}}
}
\value{
  The indices that indicate the position of the endmembers
  in the original dataset
}
\description{
  This method exhaustively checks every possible simplex
  that could be formed from p points and returns the
  indices that generated the simplex with the largest
  volume. This should indicate the endmembers based on the
  theory of N-FINDR. It should only be used for testing
  purposes as an exhaustive verification of the results
  from other N-FINDR methods as it is extremely slow for
  non-trivial datasets.
}

