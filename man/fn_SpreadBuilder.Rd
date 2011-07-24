\name{fn_SpreadBuilder}
\alias{fn_SpreadBuilder}
\title{Calculate prices of a spread from 2 instruments.}
\usage{
  fn_SpreadBuilder(prod1, prod2, ratio = 1, currency =
  "USD", from = NULL, to = NULL, session_times = NULL,
  unique_method = c("make.index.unique", "duplicated",
  "least.liq", "price.change"), silent = FALSE, ...)
}
\arguments{
  \item{prod1}{chr name of instrument that will be the 1st
  leg of a 2 leg spread}

  \item{prod2}{chr name of instrument that will be the 2nd
  leg of a 2 leg spread}

  \item{ratio}{hedge ratio. Can be a single number, or a
  vector of same length as data.}

  \item{currency}{chr name of currency denomination of the
  spread}

  \item{from}{from Date to pass through to getSymbols if
  needed.}

  \item{to}{to Date to pass through to getSymbols if
  needed.}

  \item{session_times}{ISO-8601 time subset for the session
  time, in GMT, in the format 'T08:00/T14:59'}

  \item{unique_method}{method for making the time series
  unique}

  \item{silent}{silence warnings? (FALSE by default)}

  \item{\dots}{any other passthrough parameters}
}
\value{
  an xts object with Bid, Ask, Mid columns, or Open, Close,
  Adjusted columns, or Open, Close columns. or Price
  column.
}
\description{
  It will try to get data for \code{prod1} and \code{prod2}
  from .GlobalEnv. If it cannot find the data, it will get
  it with a call to getSymbols.
}
\details{
  Prices are multiplied by multipliers and exchange rates
  to get notional values in the currency specified. The
  second leg's notional values are multiplied by the ratio.
  Then the difference is taken between the notionals of
  leg1 and the new values for leg2.

  \sQuote{make.index.unique} uses the xts function
  \code{make.index.unique} \sQuote{least.liq} subsets the
  spread time series, by using the timestamps of the leg
  that has the fewest rows. \sQuote{duplicated} removes any
  duplicate indexes. \sQuote{price.change} only return rows
  where there was a price change in the Bid, Mid or Ask
  Price of the spread.
}
\note{
  requires quantmod
}
\examples{
\dontrun{
currency("USD")
stock("SPY", "USD")
stock("DIA", "USD")
getSymbols(c("SPY","DIA"))
fSB <- fn_SpreadBuilder("SPY","DIA") #can call with names of instrument/xts ojects
fSB2 <- fn_SpreadBuilder(SPY,DIA) # or you can pass xts objects

fSB3 <- fn_SpreadBuilder("SPY","DIA",1.1) #assuming you first somehow calculated the ratio to be a constant 1.1
head(fSB)
}
}
\author{
  Lance Levenson, Brian Peterson, Garrett See
}
\seealso{
  \code{\link{buildSpread}}
  \code{\link{synthetic.instrument}}
  \code{\link{formatSpreadPrice}} \code{\link{buildRatio}}
}

