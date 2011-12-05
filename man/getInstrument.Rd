\name{getInstrument}
\alias{getInstrument}
\title{primary accessor function for getting objects of type 'instrument'}
\usage{
  getInstrument(x, Dates = NULL, silent = FALSE,
    type = "instrument")
}
\arguments{
  \item{x}{string identifier of instrument to retrieve}

  \item{Dates}{date range to retrieve 'as of', may not
  currently be implemented}

  \item{silent}{if TRUE, will not warn on failure, default
  FALSE}

  \item{type}{type of object to look for. See Details}
}
\description{
  This function will search the \code{.instrument}
  environment for objects of type \code{type}, using first
  the \code{primary_id} and then any \code{identifiers} to
  locate the instrument.  Finally, it will try adding 1 and
  then 2 dots to the \code{primary_id} to see if an
  instrument was stored there to avoid naming conflicts.
}
\details{
  \code{\link{future}} and \code{\link{option}} objects may
  have a primary_id that begins with 1 or 2 dots (in order
  to avoid naming conflics).  For example, the root specs
  for options (or futures) on the stock with ticker "SPY"
  may be stored with a primary_id of "SPY", ".SPY", or
  "..SPY".  \code{getInstrument} will try using each
  possible \code{primary_id} until it finds an instrument
  of the appropriate \code{type}
}
\examples{
\dontrun{
option('..VX', multiplier=100,
  underlying_id=future('.VX',multiplier=1000,
    underlying_id=synthetic('VIX', currency("USD"))))

getInstrument("VIX")
getInstrument('VX') #returns the future
getInstrument("VX",type='option')
getInstrument('..VX') #finds the option
}
}

