\name{predict.nfindr}
\alias{predict.nfindr}
\title{Predict Endmember Abundances}
\usage{
  \method{predict}{nfindr} (object, newdata = object$data,
    ...)
}
\arguments{
  \item{object}{The N-FINDR structure returned by the
  general \code{\link{nfindr}} interface}

  \item{newdata}{If the data stored in the object is not
  the data that should be checked for abundances then this
  parameter allows for passing in new data}

  \item{...}{Allow for extra parameters to match the
  signature of the base predict function}
}
\value{
  A matrix where the abundances for an endmember are
  returned column-wise. Each value is in the range \code{0
  - 1}
}
\description{
  Predicts the abundance percentages of each endmember at
  all sample points using the Non-Negative Least Squares
  method
}

