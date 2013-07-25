\name{nfindr99}
\alias{nfindr99}
\title{Michael E. Winter's 1999 N-FINDR unmixing algorithm}
\usage{
  nfindr99(data, p, simplex, indices, iters)
}
\arguments{
  \item{data}{Data matrix to unmix}

  \item{p}{Number of endmembers}

  \item{simplex}{The initial simplex estimation}

  \item{indices}{Indices used in the simplex estimation}

  \item{iters}{Max number of iterations}
}
\value{
  The indices that indicate the position of the endmembers
  in the original dataset
}
\description{
  This method should only be called from
  \code{\link{nfindr}}. See that function for the full
  description
}
\references{
  Michael E. Winter; "N-FINDR: an algorithm for fast
  autonomous spectral end-member determination in
  hyperspectral data", Proc.  SPIE 3753, Imaging
  Spectrometry V, 266 (October 27, 1999);
  doi:10.1117/12.366289;
}

