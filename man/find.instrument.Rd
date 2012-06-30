\name{find.instrument}
\alias{find.instrument}
\title{Find the primary_ids of instruments that contain certain strings}
\usage{
  find.instrument(text, where = "anywhere",
    Symbols = ls_instruments(), ignore.case = TRUE,
    exclude = NULL, ...)
}
\arguments{
  \item{text}{character string containing a regular
  expression.  This is used by \code{\link{grep}} (see
  also) as the \code{pattern} argument.}

  \item{where}{if \dQuote{anywhere} all levels/attributes
  of the instruments will be searched.  Otherwise,
  \code{where} can be used to specify in which
  levels/attributes to look. (e.g. \code{c("name",
  "description")} would only look for \code{text} in those
  2 places.}

  \item{Symbols}{the character ids of instruments to be
  searched. All are are searched by default.}

  \item{ignore.case}{passed to \code{\link{grep}}; if
  \code{FALSE}, the pattern matching is case sensitive and
  if \code{TRUE}, case is ignored during matching.}

  \item{exclude}{character vector of names of
  levels/attributes that should not be searched.}

  \item{...}{other arguments to pass through to
  \code{\link{grep}}}
}
\value{
  character vector of primary_ids of instruments that
  contain the sought after \code{text}.
}
\description{
  Uses regular expression matching to find
  \code{\link{instrument}}s
}
\examples{
\dontrun{
instruments.bak <- as.list(FinancialInstrument:::.instrument, all.names=TRUE)
rm_instruments(keep.currencies=FALSE)
currency("USD")
stock("SPY", "USD", description="S&P 500 ETF")
stock("DIA", "USD", description="DJIA ETF")
stock(c("AA", "AXP", "BA", "BAC", "CAT"), "USD", members.of='DJIA')
stock("BMW", currency("EUR"))
find.instrument("ETF")
find.instrument("DJIA")
find.instrument("DJIA", "members.of")
find.instrument("USD")
find.instrument("EUR")
find.instrument("EUR", Symbols=ls_stocks())
find.instrument("USD", "type")

## Can be combined with buildHierachy
buildHierarchy(find.instrument("ETF"), "type", "description")

## Cleanup. restore previous instrument environment
rm_instruments(); rm_currencies()
loadInstruments(instruments.bak)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{buildHierarchy}},
  \code{\link{instrument.table}}, \code{\link{regex}}
}

