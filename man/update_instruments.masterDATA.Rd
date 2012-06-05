\name{update_instruments.masterDATA}
\alias{update_instruments.masterDATA}
\alias{update_instruments.md}
\title{Update instrument metadata for ETFs}
\usage{
  update_instruments.masterDATA(Symbols, silent = FALSE)

  update_instruments.md(Symbols, silent = FALSE)
}
\arguments{
  \item{Symbols}{character vector of Symbols of ETFs}

  \item{silent}{silence warnings?}
}
\value{
  called for side-effect. Each ETF that is updated will be
  given instrument attributes of \dQuote{Name} and
  \dQuote{Fund.Type}
}
\description{
  Uses the masterDATA.com list of ETFs and ETNs to update
  previously defined instruments.
}
\details{
  \code{update_instruments.md} is an alias.

  MasterDATA classifies each ETF into one of six
  Fund.Types.  From their website:

  US Equity ETF: All constituents trade on a US exchange.
  Both ProShares and Rydex sponsor ETFs with the objective
  of achieving the performance (or a multiple of the
  performance) of several major US stock indexes. These
  ETFs currently are included in this category despite the
  fact that their constituent lists are generally not
  limited to US stocks.

  Global Equity ETF: One or more of the constituents do not
  trade on a US Exchange.

  Fixed Income ETF: The constituent list contains
  government and / or corporate debt instruments. ETFs with
  this classification will not be considered for inclusion
  in MasterDATA's index / ETF compilation list.

  Commodity Based ETF: This classification of ETF has no
  constituents but is structured to reflect the valuation
  of a commodity such as gold, silver, oil or interest
  rates. ETFs with this classification will not be
  considered for inclusion in MasterDATA's index / ETF
  compilation list.

  Exchange Traded Notes: A type of unsecured,
  unsubordinated debt security that was first issued by
  Barclays Bank PLC. The purpose of ETNs is to create a
  type of security that combines both the aspects of bonds
  and exchange traded funds (ETF). Similar to ETFs, ETNs
  are traded on a major exchange.
}
\examples{
\dontrun{
stock(s <- c("SPY", "DIA"), currency("USD"))
update_instruments.masterDATA(s)
buildHierarchy(s, "Name", "Fund.Type", "defined.by")
}
}
\author{
  Garrett See
}
\references{
  \url{http://masterDATA.com}
  (\url{http://www.masterdata.com/helpfiles/ETF_List_Downloads/AllTypes.csv})
}
\seealso{
  \code{\link{update_instruments.yahoo}},
  \code{\link{update_instruments.instrument}}
}

