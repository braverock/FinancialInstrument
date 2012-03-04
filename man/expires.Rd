\name{expires}
\alias{expires}
\title{extract the correct expires value from an \code{instrument}}
\usage{
  expires(x, ...)
}
\arguments{
  \item{x}{instrument or name of instrument}

  \item{...}{arguments to be passed to methods}
}
\value{
  an expiration \code{Date}
}
\description{
  Currently, there are methods for \code{instrument},
  \code{spread}, \code{character}, and \code{xts}
}
\details{
  Will return either the last expiration date before a
  given \code{Date}, or the first expiration date after a
  given \code{Date} (if \code{expired==FALSE}).

  If an \code{\link{instrument}} contains a value for
  expires that does not include a day (e.g. "2012-03"), or
  if the expires value is estimated from a
  \code{future_series} primary_id, it will be assumed that
  the \code{instrument} expires on the first of the month
  (i.e. if the expires value of an instrument were
  "2012-03", or if there were no expires value but the
  suffix_id were "H12", the value returned would be
  "2012-03-01"). Note that most non-energy future_series
  expire after the first of the month indicated by their
  suffix_id and most energy products expire in the month
  prior to their suffix_id month.
}
\examples{
\dontrun{
instr <- instrument("FOO_U1", currency=currency("USD"), multiplier=1,
                    expires=c("2001-09-01", "2011-09-01", "2021-09-01"),
                    assign_i=FALSE)
#Last value of expires that's not after Sys.Date
expires(instr)
# First value of expires that hasn't already passed.
expires(instr, expired=FALSE)
# last value that's not after 2011-01-01
expires(instr, Date="2011-01-01")
# first value that's not before 2011-01-01
expires(instr, Date="2011-01-01", expired=FALSE)

## expires.character
expires("FOO_U1") # warning that FOO_U1 is not defined
instrument("FOO_U1", currency=currency("USD"), multiplier=1,
           expires=c("2001-09-01", "2011-09-01", "2021-09-01"),
           assign_i=TRUE)
expires("FOO_U1")
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{expires.instrument}},
  \code{\link{expires.character}}, \code{\link{sort_ids}}

  \code{\link{getInstrument}} and
  \code{\link{buildHierarchy}} to see actual values stored
  in \code{instrument}
}

