\name{FindCommonInstrumentAttributes}
\alias{FindCommonInstrumentAttributes}
\title{Find attributes that more than one instrument have in common}
\usage{
  FindCommonInstrumentAttributes(Symbols, ...)
}
\arguments{
  \item{Symbols}{character vector of primary_ids of
  instruments}

  \item{\dots}{arguments to pass to
  \code{\link[FinancialInstrument]{getInstrument}}}
}
\value{
  character vector of names of attributes that all
  \code{Symbols}' instruments have in common
}
\description{
  Find attributes that more than one instrument have in
  common
}
\note{
  I really do not like the name of this function, so if it
  survives, its name may change
}
\examples{
\dontrun{
ibak <- as.list(FinancialInstrument:::.instrument, all.names=TRUE)
Symbols <- c("SPY", "AAPL")
define_stocks(Symbols, addIBslot=FALSE)
update_instruments.SPDR("SPY")
update_instruments.TTR("AAPL", exchange="NASDAQ")
FindCommonInstrumentAttributes(Symbols)
FindCommonInstrumentAttributes(c(Symbols, "USD"))
reloadInstruments(ibak)
}
}
\author{
  gsee
}

