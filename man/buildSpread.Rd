\name{buildSpread}
\alias{buildSpread}
\title{construct a price/level series for pre-defined multi-leg spread instrument}
\usage{
  buildSpread(spread_id, Dates = NULL, onelot = TRUE,
  prefer = NULL, auto.assign = TRUE, env = .GlobalEnv)
}
\arguments{
  \item{spread_id}{The name of the instrument that contains
  members and memberratio}

  \item{Dates}{date range to subset on, will be used for
  \code{\link[quantmod]{getSymbols}} if the instrument is
  not available via \code{\link{get}}}

  \item{onelot}{Should the series be divided by the first
  leg's ratio?}

  \item{prefer}{price column to use to build structure.}

  \item{auto.assign}{assign the spread? If FALSE, the xts
  object will be returned}

  \item{env}{environment in which to assign spread.}
}
\value{
  If \code{auto.assign} is FALSE, a univariate xts object.
  Otherwise, the xts object will be assigned to
  \code{spread_id} and the \code{spread_id} will be
  returned.
}
\description{
  build price series for spreads, butterflies, or other
  synthetic instruments, using metadata of a previously
  defined synthetic instrument.
}
\details{
  The spread and all legs must be defined instruments.

  This function can build multileg spreads such as
  calendars, butterflies, condors, etc. However, the
  returned series will be univariate. It does not build Bid
  Ask Mid data like fn_SpreadBuilder does.
}
\note{
  this could also be used to build a basket or a strip by
  using only positive values in memberratio
}
\examples{
\dontrun{
currency("USD")
stock("SPY","USD",1)
stock("DIA","USD",1)
getSymbols(c("SPY","DIA"))

spread("SPYDIA", "USD", c("SPY","DIA"),c(1,-1)) #define it.
buildSpread('SPYDIA') #build it.
head(SPYDIA)
}
}
\author{
  Brian Peterson, Garrett See
}
\seealso{
  \code{\link{fn_SpreadBuilder}} \code{\link{spread}} for
  instructions on defining the spread
}

