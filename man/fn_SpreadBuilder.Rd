\name{fn_SpreadBuilder}
\alias{fn_SpreadBuilder}
\title{Calculate prices of a spread from 2 instruments.}
\usage{
  fn_SpreadBuilder(prod1, prod2, ratio = 1,
    currency = "USD", from = NULL, to = NULL,
    session_times = NULL, notional = TRUE,
    unique_method = c("make.index.unique", "duplicated", "least.liq", "price.change"),
    silent = FALSE, auto.assign = TRUE, env = .GlobalEnv,
    ...)
}
\arguments{
  \item{prod1}{chr name of instrument that will be the 1st
  leg of a 2 leg spread (Can also be xts data for first
  product)}

  \item{prod2}{chr name of instrument that will be the 2nd
  leg of a 2 leg spread (Can also be xts data for second
  product)}

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

  \item{notional}{TRUE/FALSE. Should the prices be
  multiplied by contract multipliers before calculating the
  spread?}

  \item{unique_method}{method for making the time series
  unique}

  \item{auto.assign}{if \code{TRUE} (the default) the
  constructed spread will be stored in symbol created with
  \code{\link{make_spread_id}}. instrument metadata will
  also be created and stored with the same primary_id.}

  \item{env}{if \code{prod1} and \code{prod1} are
  character, this is where to \code{get} the data.  Also,
  if \code{auto.assign} is \code{TRUE} this is the
  environment in which to store the data (.GlobalEnv by
  default)}

  \item{silent}{silence warnings? (FALSE by default)}

  \item{\dots}{other arguments to pass to \code{getSymbols}
  and/or \code{\link{make_spread_id}}}
}
\value{
  an xts object with Bid, Ask, Mid columns, or Open, Close,
  Adjusted columns, or Open, Close columns. or Price
  column.
}
\description{
  Given 2 products, calculate spread values for as many
  columns as practicable
}
\details{
  \code{prod1} and \code{prod2} can be the names of
  instruments, or the xts objects themselves.
  Alternatively, \code{prod2} can be omitted, and a vector
  of 2 instrument names can be given to \code{prod1}. See
  the last example for this usage.

  If \code{prod1} and \code{prod2} are names (not xts
  data), it will try to get data for \code{prod1} and
  \code{prod2} from \code{env} (.GlobalEnv by default).  If
  it cannot find the data, it will get it with a call to
  getSymbols. Prices are multiplied by multipliers and
  exchange rates to get notional values in the currency
  specified. The second leg's notional values are
  multiplied by \code{ratio}. Then the difference is taken
  between the notionals of leg1 and the new values for
  leg2.

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

# Call fn_SpreadBuilder with vector of 2 instrument names
# in 1 arg instead of using both prod1 and prod2.
fSB4 <- fn_SpreadBuilder(c("SPY","DIA"))
#download data and plot the closing values of a spread in one line
chartSeries(Cl(fn_SpreadBuilder(getSymbols(c("SPY","DIA")),auto.assign=FALSE)))
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

