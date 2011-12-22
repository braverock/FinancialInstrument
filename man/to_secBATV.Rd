\name{to_secBATV}
\alias{to_secBATV}
\title{convert tick data to one-second data}
\usage{
  to_secBATV(x)
}
\arguments{
  \item{x}{the xts series to convert to 1 minute BATMV}
}
\value{
  an xts object of 1 second frequency
}
\description{
  From tick data with columns: \dQuote{Price},
  \dQuote{Volume}, \dQuote{Bid.Price}, \dQuote{Bid.Size},
  \dQuote{Ask.Price}, \dQuote{Ask.Size}, to data of one
  second frequency with columns \dQuote{Bid.Price},
  \dQuote{Bid.Size}, \dQuote{Ask.Price}, \dQuote{Ask.Size},
  \dQuote{Trade.Price}, and \dQuote{Volume}
}
\details{
  The primary purpose of this function is to reduce the
  amount of data on disk so that it will take less time to
  load the data into memory.

  If there are no trades or bid/ask price updates in a
  given second, we will not make a row for that timestamp.
  If there were no trades, but the bid or ask price
  changed, then we _will_ have a row but the Volume and
  Trade.Price will be NA.
}
\examples{
\dontrun{
getSymbols("CLU1")
system.time(xsec <- to_secBATV(CLU1))
}
}
\author{
  gsee
}

