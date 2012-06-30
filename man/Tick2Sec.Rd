\name{to_secBATV}
\alias{alltick2sec}
\alias{to_secBATV}
\title{Convert tick data to one-second data}
\usage{
  to_secBATV(x)

  alltick2sec(getdir = "~/TRTH/tick/",
    savedir = "~/TRTH/sec/", Symbols = list.files(getdir),
    overwrite = FALSE)
}
\arguments{
  \item{x}{the xts series to convert to 1 minute BATV}

  \item{getdir}{Directory that contains tick data}

  \item{savedir}{Directory in which to save converted data}

  \item{Symbols}{String names of instruments to convert}

  \item{overwrite}{TRUE/FALSE. If file already exists in
  savedir, should it be overwritten?}
}
\value{
  \code{to_secBATV} returns an xts object of one second
  frequency.  \code{alltick2sec} returns a list of files
  that were converted.
}
\description{
  This is like taking a snapshot of the market at the end
  of every second, except the volume over the second is
  summed.
}
\details{
  From tick data with columns: \dQuote{Price},
  \dQuote{Volume}, \dQuote{Bid.Price}, \dQuote{Bid.Size},
  \dQuote{Ask.Price}, \dQuote{Ask.Size}, to data of one
  second frequency with columns \dQuote{Bid.Price},
  \dQuote{Bid.Size}, \dQuote{Ask.Price}, \dQuote{Ask.Size},
  \dQuote{Trade.Price}, and \dQuote{Volume}

  The primary purpose of these functions is to reduce the
  amount of data on disk so that it will take less time to
  load the data into memory.

  If there are no trades or bid/ask price updates in a
  given second, we will not make a row for that timestamp.
  If there were no trades, but the bid or ask price
  changed, then we _will_ have a row but the Volume and
  Trade.Price will be NA.

  If there are multiple trades in the same second, Volume
  will be the sum of the volume, but only the last trade
  price in that second will be printed. Similarly, if there
  is a trade, and then later in the same second, there is a
  bid/ask update, the last Bid/Ask Price/Size will be used.

  \code{alltick2sec} is used to convert the data of several
  files from tick to one second frequency data.
}
\note{
  \code{to_secBATV} is used by the TRTH_BackFill.R script
  in the inst/parser directory of the FinancialInstrument
  package.  These functions are specific to to data created
  by that script and are not intended for more general use.
}
\examples{
\dontrun{
getSymbols("CLU1")
system.time(xsec <- to_secBATV(CLU1))
convert.log <- alltick2sec()
}
}
\author{
  gsee
}

