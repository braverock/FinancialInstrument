\name{update_instruments.morningstar}
\alias{update_instruments.morningstar}
\alias{update_instruments.ms}
\title{Update instrument metadata for ETFs}
\usage{
  update_instruments.morningstar(Symbols, silent = FALSE)

  update_instruments.ms(Symbols, silent = FALSE)
}
\arguments{
  \item{Symbols}{character vector of Symbols of ETFs}

  \item{silent}{silence warnings?}
}
\value{
  called for side-effect.
}
\description{
  Currently, this only updates ETFs.  It will add
  \dQuote{msName} and \dQuote{msCategory} attributes to the
  instruments. (ms for morningstar)
}
\examples{
\dontrun{
## backup .instrument environment
ibak <- as.list(FinancialInstrument:::.instrument)
rm_instruments()
stock(s <- c("SPY", "USO", "LQD"), currency("USD"))
update_instruments.morningstar(s)
instrument.table(s)
## cleanup and restore instrument environment
rm_instruments(keep.currencies=FALSE)
loadInstruments(ibak)
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.morningstar.com}
}
\seealso{
  \code{\link{update_instruments.yahoo}},
  \code{\link{update_instruments.TTR}}
  \code{\link{update_instruments.iShares}}
}

