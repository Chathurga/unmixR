\name{nfindr}
\alias{nfindr}
\title{General interface to N-FINDR spectral unmixing implementations}
\usage{
  nfindr(...)

  \method{nfindr}{default} (data, p, method = "LDU",
    indices = sample(nrow(data), p), ..., drop = FALSE)

  \method{nfindr}{formula} (formula, frame, p,
    method = "LDU", indices = sample(nrow(data), p), ...,
    drop = FALSE)
}
\arguments{
  \item{data}{Data to unmix. It will be converted to a
  matrix using as.matrix. The matrix should contain a
  spectrum per row. This data will be dimensionally reduced
  using PCA. If you want to reduce the data using some
  other method then reduce it first and set drop to
  \code{TRUE}}

  \item{formula}{Formula object}

  \item{frame}{Data frame containing the hyperspectral
  data}

  \item{p}{Number of endmembers}

  \item{method}{The N-FINDR algorithm to use. Options:
  \itemize{ \item 99 (\code{\link{nfindr99}}) \item LDU
  (\code{\link{nfindrLDU}}) \item SeqLDU
  (\code{\link{nfindrSeqLDU}}) \item Brute
  (\code{\link{nfindrBrute}}) } Default: LDU as it
  generally performs the best}

  \item{indices}{Locations of the rows in the dataset that
  will be used to form the initial simplex. Default:
  Randomly selected indices}

  \item{...}{Extra parameters that will get passed into
  selected method, see selected method for options}

  \item{drop}{Boolean that indicates whether the
  \code{data} parameter should be stored in the resulting
  structure. This should only be set to \code{True} when
  \code{data} was passed in already reduced}
}
\value{
  A structure which contains: \itemize{ \item
  \strong{data}: the original data or reduced data if drop
  is set to \code{True} \item \strong{indices}: the indices
  of the spectra which increased the simplex volume the
  most. These are the indices of the endmembers. If drop is
  set to \code{True} then indices will be 1 to p }
}
\description{
  All the N-FINDR techniques are based on the fact that, in
  N spectral dimensions, the N-volume contained by a
  simplex formed of the purest pixels is larger than any
  other volume formed from any other combination of pixels.
}

