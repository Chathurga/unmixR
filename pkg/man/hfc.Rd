\name{hfc}
\alias{hfc}
\title{Harsanyi-Farrand-Chang method for estimating the number of endmembers in
a spectral image}
\usage{
  hfc(data, alpha)
}
\arguments{
  \item{data}{Spectra matrix to examine}

  \item{alpha}{False-alarm probability (can be a vector of
  alpha values)}
}
\value{
  The estimated number of endmembers (in the form vector if
  alpha is a vector)
}
\description{
  Calculates the difference between the eigenvalues in the
  correlation and covariance matrices of the spectra to
  determine the virtual dimensionality (VD)
}

