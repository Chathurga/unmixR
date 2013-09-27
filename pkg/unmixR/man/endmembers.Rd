\name{endmembers}
\alias{endmembers}
\title{Retrieve N-FINDR/VCA Endmembers}
\usage{
  endmembers(object, newdata = object$data)
}
\arguments{
  \item{object}{The N-FINDR/VCA structure returned by the
  general \code{\link{nfindr}}/\code{\link{vca}} interface}

  \item{newdata}{If the data stored in the object does not
  actually contain the endmembers then this parameter
  allows for passing in new data}
}
\value{
  A matrix where each row is an endmember as calculated by
  the unmixing algorithm
}
\description{
  Retrieves the endmembers from a dataset using the model
  returned by \code{\link{nfindr}} or \code{\link{vca}}
}

