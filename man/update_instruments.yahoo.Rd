\name{update_instruments.yahoo}
\alias{update_instruments.TTR}
\alias{update_instruments.yahoo}
\title{updates instrument metadata with data from yahoo}
\usage{
  update_instruments.yahoo(Symbols = c("stocks", "all"),
  verbose = FALSE)

  update_instruments.TTR(Symbols = c("stocks", "all"),
  exchange = c("AMEX", "NASDAQ", "NYSE"))
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
}
\value{
  called for side-effect
}
\description{
  Adds/updates information in instrument with data
  downloaded from yahoo
}
\details{
  if you call \code{update_instruments.yahoo} with one of
  \sQuote{all} or \sQuote{stocks}, it is the same as
  calling it with the relevant ls_ function (e.g.
  \code{ls_stocks()}).  Therefore, functionality can be
  extended by using ls_ functions instead of a descriptive
  string.
}
\examples{
\dontrun{
stock('GS',currency('USD'))
update_instruments.yahoo('GS')
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

