\name{hfc}
\alias{hfc}
\title{Harsanyi-Farrand-Chang method for estimating the number of endmembers in
a spectral image}
\usage{
  hfc(data, alpha)

  hfc(data, alpha)
}
\arguments{
  \item{data}{Spectra matrix to examine

  Spectra matrix with spectra in rows, frequencies in
  columns.}

  \item{alpha}{False-alarm probability (can be a vector of
  alpha values)

  NA}

  \item{alpha}{False-alarm probability (can be a vector of
  alpha values)}
}
\value{
  The estimated number of endmembers (in the form vector if
  alpha is a vector)

  The estimated number of endmembers (in the form vector if
  alpha is a vector)
}
\description{
  Calculates the difference between the eigenvalues in the
  correlation and covariance matrices of the spectra to
  determine the virtual dimensionality (VD)

  Calculates the difference between the eigenvalues in the
  correlation and covariance matrices of the spectra to
  determine the virtual dimensionality (VD)
}

