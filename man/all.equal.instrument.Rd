\name{all.equal.instrument}
\alias{all.equal.instrument}
\title{instrument all.equal method}
\usage{
  \method{all.equal}{instrument} (target, current,
    char.n = 2, collapse = ";", ...)
}
\arguments{
  \item{char.n}{If length of a character vector is
  \code{char.n} or less it will be treated as a single
  element. A negative value for \code{char.n} will be
  treated as if it were positive \code{Inf}.}

  \item{collapse}{Only used if a character vector is of
  length less than \code{char.n}.  Unless \code{collapse}
  is \code{NULL}, it will be used in a call to
  \code{\link{paste}}.  If \code{collapse} is \code{NULL},
  each element of the character vector will be compared
  separately.}
}
\description{
  instrument all.equal method
}
\note{
  ALPHA code. Subject to change
}
\examples{
\dontrun{
currency("USD")
stock("SPY", "USD", validExchanges=c("SMART", "ARCA", "BATS", "BEX"))
stock("DIA", "USD", validExchanges=c("SMART", "ARCA", "ISLAND"),
     ExtraField="something")

all.equal(getInstrument("SPY"), getInstrument("DIA"))
all.equal(getInstrument("SPY"), getInstrument("DIA"), char.n=5)
all.equal(getInstrument("SPY"), getInstrument("DIA"), char.n=5, collapse=NULL)

all.equal(getInstrument("DIA"), getInstrument("USD"))
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{getInstrument}},
  \code{\link{instrument.table}},
  \code{\link{buildHierarchy}}
}
\keyword{internal}
\keyword{utilities}

