\name{update_instruments.iShares}
\alias{update_instruments.iShares}
\alias{update_instruments.SPDR}
\title{update iShares and SPDR ETF metadata}
\usage{
  update_instruments.iShares(Symbols, silent = FALSE)

  update_instruments.SPDR(Symbols, silent = FALSE)
}
\arguments{
  \item{Symbols}{character vector of iShares ETF ticker
  symbols.  If not specified, \code{unique(c(ls_funds(),
  ls_stocks()))} will be used.}

  \item{silent}{silence the warning that no iShares are
  defined?}
}
\value{
  called for side-effect
}
\description{
  This will update previously defined iShares or SPDR ETF
  \code{instrument}s. Both functions will add attributes
  for \dQuote{Name}, and \dQuote{FundFamily}
  (\dQuote{iShares} or \dQuote{SPDR}).
  \code{update_instruments.iShares} will also add an
  attribute for \dQuote{MgmtFees}
}
\note{
  \code{update_instruments.SPDR} will probably NOT work on
  Windows because in the call to \code{download.file} it
  uses \code{method=curl} since it has to download from an
  https URL scheme.
}
\examples{
\dontrun{
stock("IWC", currency("USD"))
update_instruments.iShares("IWC")
getInstrument("IWC")

Symbols <- stock(c("SPY", "JNK"), currency("USD"))
update_instruments.SPDR(Symbols)
buildHierarchy(c("SPY", "JNK"), "Name")
}
}
\author{
  Garrett See
}
\references{
  \url{http://us.ishares.com/home.htm},
  \url{https://www.spdrs.com/}
}
\seealso{
  \code{update_instruments.yahoo},
  \code{update_instruments.TTR},
  \code{twsInstrument:::update_instruments.IB},
  \code{update_instruments.instrument},
  \code{\link{update_instruments.morningstar},
  \code{\link{update_instruments.masterDATA}
}

