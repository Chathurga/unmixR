\name{predict.nfindr}
\alias{predict.nfindr}
\title{Predict Endmember Abundances}
\usage{
  \method{predict}{nfindr} (object, ...)
}
\arguments{
  \item{object}{The N-FINDR structure returned by the
  general \code{\link{nfindr}} interface}
}
\value{
  A matrix where the abundances for an endmember are
  returned column-wise. Each value is in the range \code{0
  - 1}
}
\description{
  Predicts the abundance precentages of each endmember at
  all sample points using the Non-Negative Least Squares
  method
}

