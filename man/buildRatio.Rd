\name{buildRatio}
\alias{buildRatio}
\title{contruct price ratios of 2 instruments}
\usage{
  buildRatio(x, env = .GlobalEnv, silent = FALSE)
}
\arguments{
  \item{x}{vector of instrument names. e.g. c("SPY","DIA")}

  \item{env}{environment where xts data is stored}

  \item{silent}{silence warnings?}
}
\value{
  An xts object with columns of Bid, Ask, Mid OR Open,
  Close, Adjusted OR Price
}
\description{
  Calculates time series of ratio of 2 instruments using
  available data. Returned object will be ratios calculated
  using Bids, Asks, and Mids, or Opens, Closes, and
  Adjusteds.
}
\details{
  \code{x} should be a vector of 2 instrument names. An
  attempt will be made to \code{get} the data for both
  instruments.  If there are no xts data stored under
  either of the names, it will try to return prebuilt data
  with a call to \code{\link{.get_rate}}.

  If the data are not of the same frequency, or are not of
  the same type (OHLC, BBO, etc.) An attempt will be made
  to make them compatible.  Preference is given to the
  first leg.

  If the data in \code{x[1]} is daily or slower and the
  data in \code{x[2]} is intraday (e.g. if you give it
  daily OHLC and intraday Bid Ask Mid, it will use all of
  the OHLC columns of \code{x[1]} and only the the End of
  Day Mid price of the BAM object.

  If the data in \code{x[1]} is intraday, and the data in
  \code{x[2]} is daily or slower, for each day, the
  previous closing value of \code{x[2]} will be filled
  forward with \code{na.locf}
}
\examples{
\dontrun{
syms <- c("SPY","DIA")
getSymbols(syms)
rat <- buildRatio(syms)
summary(rat)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{redenominate}} \code{\link{buildSpread}}
  \code{\link{fn_SpreadBuilder}}
}

