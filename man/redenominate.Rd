\name{redenominate}
\alias{redenominate}
\title{Redenominate (change the base of) an instrument}
\usage{
  redenominate(x, new_base = "USD", old_base = NULL,
    EOD_time = "15:00:00", env = .GlobalEnv,
    silent = FALSE)
}
\arguments{
  \item{x}{can be either an xts object or the name of an
  instrument.}

  \item{new_base}{change the denomination to this; usually
  a currency.}

  \item{old_base}{what is the current denomination?}

  \item{EOD_time}{If data need to be converted to daily,
  this is the time of day to take the observation.}

  \item{env}{environment that holds the data}

  \item{silent}{silence warnings?}
}
\value{
  xts object, with as many columns as practicable, that
  represents the value of an instrument in a different
  currency (base).
}
\description{
  Redenominate (change the base of) an instrument
}
\details{
  If \code{old_base} is not provided, \code{x} must be the
  name of an instrument (or an object with the name of a
  defined instrument) so that the currency attribute of the
  instrument can be used.  Otherwise, \code{old_base} must
  be provided.

  If you want to convert to JPY something that is
  denominated in EUR, you must have data for the EURJPY (or
  JPYEUR) exchange rate. If you don't have data for EURJPY,
  but you do have data for EURUSD and USDJPY, you could
  \code{redenominate} to USD, then \code{redenominate} to
  EUR, but this function is not yet smart enough to do that
  for you.

  See the help for buildRatio also.
}
\note{
  this does not yet define any instruments or assign
  anything.
}
\examples{
\dontrun{
require(quantmod)
EURUSD <- getSymbols("EURUSD=x",src='yahoo',auto.assign=FALSE)
GLD <- getSymbols("GLD", src='yahoo', auto.assign=FALSE)
GLD.EUR <- redenominate(GLD,"EUR","USD") #can call with xts object

currency("USD")
stock("GLD","USD")
GLD.EUR <- redenominate('GLD','EUR') #can also call with instrument name
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{buildRatio}}
}

