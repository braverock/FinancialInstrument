\name{add.identifier}
\alias{add.identifier}
\title{add an identifier to an \code{instrument}}
\usage{
  add.identifier(primary_id, identifier, ...)
}
\arguments{
  \item{primary_id}{primary_id of an
  \code{\link{instrument}}}

  \item{identifier}{the identifier to add.  Either a list,
  or a string that will be coerced to a list.}

  \item{...}{parameters to pass through to
  \code{\link{getInstrument}}}
}
\value{
  called for side-effect
}
\description{
  add an identifier to an \code{instrument}
}
\examples{
\dontrun{
stock("XXX", currency("USD"))
add.identifier("XXX", list(yahoo="^XXX"))
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

