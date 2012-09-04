\name{update_instruments.yahoo}
\alias{update_instruments.TTR}
\alias{update_instruments.yahoo}
\title{updates instrument metadata with data from yahoo}
\usage{
  update_instruments.yahoo(Symbols = c("stocks", "all"),
    verbose = FALSE)

  update_instruments.TTR(Symbols = c("stocks", "all"),
    exchange = c("AMEX", "NASDAQ", "NYSE"), silent = FALSE)
}
\arguments{
  \item{Symbols}{can be a vector of instrument names, or,
  can be \sQuote{all} or \sQuote{stocks} or, for
  update_instruments.TTR, can be NULL in which case all
  stocks found with \code{stockSymbols} will be defined}

  \item{exchange}{character vector of names of exchanges.
  Used in \sQuote{TTR} method. Can be \dQuote{AMEX},
  \dQuote{NASDAQ}, or \dQuote{NYSE}}

  \item{verbose}{be verbose?}

  \item{silent}{silence warnings?}
}
\value{
  called for side-effect
}
\description{
  Adds/updates information in instrument with data
  downloaded from yahoo
}
\details{
  Although these functions are intended to update the
  metadata of previously defined instruments,
  \code{update_instruments.TTR} will define the stocks if
  they do not already exist.

  \code{update_instruments.TTR} is only to be used on U.S.
  stocks denominated in USD.
}
\examples{
\dontrun{
	stock('GS',currency('USD'))
 update_instruments.yahoo('GS')
	getInstrument('GS')
 update_instruments.TTR('GS')
 getInstrument('GS')
}
}
\author{
  Garrett See
}
\references{
  Yahoo! Finance \url{finance.yahoo.com} YahooQuote
  \url{http://dirk.eddelbuettel.com/code/yahooquote.html}
  gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm}
}
\seealso{
  \code{\link{update_instruments.instrument}},
  \code{\link{update_instruments.morningstar},
  \code{\link{update_instruments.masterDATA},
  \code{\link[TTR]{stockSymbols}}, \code{\link{stock}}
}

