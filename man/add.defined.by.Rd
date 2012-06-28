\name{add.defined.by}
\alias{add.defined.by}
\title{Add a source to the defined.by field of an \code{instrument}}
\usage{
  add.defined.by(primary_ids, ...)
}
\arguments{
  \item{primary_ids}{character vector of primary_ids of
  \code{\link{instrument}}s}

  \item{...}{strings, or character vector, or semi-colon
  delimited string.}
}
\value{
  called for side-effect
}
\description{
  Concatenate a string or strings (passed through dots) to
  the defined.by field of an instrument (separated by
  semi-colons).  Any duplicates will be removed.  See
  Details.
}
\details{
  If there is already a value for the \code{defined.by}
  attribute of the \code{primary_id} instrument, that
  string will be split on semi-colons and converted to a
  character vector.  That will be \code{c}ombined with any
  new strings (in \code{...}).  The unique value of this
  new vector will then be converted into a semi-colon
  delimited string that will be assigned to the
  \code{defined.by} attribute of the \code{primary_ids}'
  instruments

  Many functions that create or update instrument
  definitions will also add or update the value of the
  defined.by attribute of that instrument.  If an
  instrument has been updated by more than one function,
  it's \code{defined.by} attribute will likely be a
  semi-colon delimited string (e.g. \dQuote{TTR;yahoo}).
}
\examples{
\dontrun{
update_instruments.TTR("GS")
getInstrument("GS")$defined.by #TTR
add.defined.by("GS", "gsee", "demo")
add.defined.by("GS", "gsee;demo") #same
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{add.identifier}},
  \code{\link{instrument_attr}}
}

