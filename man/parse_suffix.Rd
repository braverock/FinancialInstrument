\name{parse_suffix}
\alias{parse_suffix}
\title{parse a suffix_id}
\usage{
  parse_suffix(x, silent = TRUE)
}
\arguments{
  \item{x}{the suffix_id to be parsed}

  \item{silent}{silence warnings? (warning will usually be
  about inferring a 4 digit year from a 1 or 2 digit year)}
}
\value{
  an object of class \sQuote{suffix.list} which is a list
  containing \sQuote{type} of instrument, \sQuote{month} of
  expiration, \sQuote{year} of expiration, \sQuote{strike}
  price of option, \sQuote{right} of option (\dQuote{C} or
  \dQuote{P}), \sQuote{cm} (maturity in days of a constant
  maturity contract), \sQuote{cc} (method for calculating a
  continuous contract), \sQuote{format} (string that
  indicates the format of the unparsed id).
}
\description{
  extract information from the suffix_id of an instrument
}
\details{
  These would be recognized as a Sep 2011 outright futures
  contract: U1, U11, SEP1, SEP11, U2011, Sep2011, SEP2011

  These would be recognized as a call with a strike of
  122.5 that expires Sep 17, 2011: 110917C122.5,
  20110917C122.5, 110917C00122500, 20110917C00122500

  These would be recognized as Sep 2011 single stock
  futures: 1CU1, 1CU11, 1CSEP11, 1DU1 (dividend protected)

  These would be recognized as Adjusted futures: cm.30 (30
  day constant maturity future), cc.OI (continuous contract
  rolled when Open Interest rolls), cc.Vol (continuous
  contract roll when Volumn rolls), cc.Exp.1 (continuous
  contract rolled 1 day before Expiration)

  Synthetics and spreads:

  SPY.DIA --> type == synthetic;

  U1.Z1 or U11.Z11 --> type == "calendar", "spread"; month
  == 'SEP', year == 2011

  U1.0302 --> type == "ICS", "spread"; month == 'SEP', year
  == 2011

  110917C125.110917P125 --> type == option_spread, spread
}
\examples{
parse_suffix("U11")
parse_suffix("110917C125")
}
\author{
  Garrett See
}
\seealso{
  \code{\link{parse_id}}, \code{\link{format_id}}, \code
}

