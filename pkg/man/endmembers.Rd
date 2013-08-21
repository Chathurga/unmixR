\name{endmembers}
\alias{endmembers}
\title{Retrieve N-FINDR Endmembers}
\usage{
  endmembers(object, newdata = object$data)
}
\arguments{
  \item{object}{The N-FINDR structure returned by the
  general \code{\link{nfindr}} interface}

  \item{newdata}{If the data stored in the object does not
  actually contain the endmembers then this parameter
  allows for passing in new data}
}
\value{
  A matrix where each row is an endmember as calculated by
  N-FINDR
}
\description{
  Retrieves the endmembers from a dataset using the model
  returned by \code{\link{nfindr}}
}

