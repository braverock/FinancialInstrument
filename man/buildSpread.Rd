\name{buildSpread}
\alias{buildBasket}
\alias{buildSpread}
\title{Construct a price/level series for pre-defined multi-leg spread instrument}
\usage{
  buildSpread(spread_id, Dates = NULL, onelot = TRUE,
    prefer = NULL, auto.assign = TRUE, env = .GlobalEnv)

  buildBasket(spread_id, Dates = NULL, onelot = TRUE,
    prefer = NULL, auto.assign = TRUE, env = .GlobalEnv)
}
\arguments{
  \item{spread_id}{The name of the \code{instrument} that
  contains members and memberratio}

  \item{Dates}{Date range on which to subset.  Also, if a
  member's data is not available via \code{\link{get}}
  \code{\link[quantmod]{getSymbols}} will be called, and
  the values of the \code{from} and \code{to} arguments
  will be determined using \code{\link[xts]{.parseISO8601}}
  on \code{Dates}.}

  \item{onelot}{Should the series be divided by the first
  leg's ratio?}

  \item{prefer}{Price column to use to build structure.}

  \item{auto.assign}{Assign the spread? If FALSE, the xts
  object will be returned.}

  \item{env}{Environment holding data for members as well
  as where spread data will be assigned.}
}
\value{
  If \code{auto.assign} is FALSE, a univariate xts object.
  Otherwise, the xts object will be assigned to
  \code{spread_id} and the \code{spread_id} will be
  returned.
}
\description{
  Build price series for spreads, butterflies, or other
  synthetic instruments, using metadata of a previously
  defined synthetic instrument.
}
\details{
  The spread and all legs must be defined instruments.

  This function can build multileg spreads such as
  calendars, butterflies, condors, etc. However, the
  returned series will be univariate. It does not return
  multiple columns (e.g. \sQuote{Bid}, \sQuote{Ask},
  \sQuote{Mid}) like \code{\link{fn_SpreadBuilder}} does.

  \code{buildBasket} is an alias

  TODO: allow for multiplier (divisor) that is a vector.
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

