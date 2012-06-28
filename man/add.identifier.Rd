\name{add.identifier}
\alias{add.identifier}
\title{Add an identifier to an \code{instrument}}
\usage{
  add.identifier(primary_id, ...)
}
\arguments{
  \item{primary_id}{primary_id of an
  \code{\link{instrument}}}

  \item{...}{identifiers passed as regular named
  arguments.}
}
\value{
  called for side-effect
}
\description{
  Add an identifier to an \code{\link{instrument}} unless
  the instrument already has that identifier.
}
\examples{
\dontrun{
stock("XXX", currency("USD"))
add.identifier("XXX", yahoo="^XXX")
getInstrument("^XXX")
add.identifier("^XXX", "x3")
all.equal(getInstrument("x3"), getInstrument("XXX")) #TRUE
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{instrument_attr}}
}

