\name{.repvec.row}
\alias{.repvec.row}
\title{Repeat a Vector Row-Wise}
\usage{
  .repvec.row(v, n)
}
\arguments{
  \item{v}{The vector that should be repeated}

  \item{n}{The number of rows in the resulting matrix}
}
\value{
  A matrix with \code{n} rows where each row is \code{v}
}
\description{
  Takes in a vector and repeats it column-wise to make a
  matrix with \code{n} rows
}

