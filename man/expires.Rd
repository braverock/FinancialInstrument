\name{expires}
\alias{expires}
\title{extract the correct expires value from an \code{instrument}}
\usage{
  expires(x, ...)
}
\arguments{
  \item{x}{instrument or name of instrument}

  \item{...}{not in use}
}
\value{
  character string representation of an expiration date
}
\description{
  Currently, there are methods for \code{instrument} and
  \code{character}
}
\details{
  Will return either the last expiration date before a
  given Date, or the first expiration date after a given
  Date (if \code{expired==FALSE}).
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
  \code{\link{expires.character}},
  \code{\link{getInstrument}}
}

