\name{exchange_rate}
\alias{exchange_rate}
\title{constructor for spot exchange rate instruments}
\usage{
  exchange_rate(primary_id = NULL, currency = NULL,
    counter_currency = NULL, identifiers = NULL, ...)
}
\arguments{
  \item{primary_id}{string identifier, usually expressed as
  a currency pair 'USDYEN' or 'EURGBP'}

  \item{currency}{string identifying the currency the
  exchange rate ticks in}

  \item{counter_currency}{string identifying the currency
  which the rate uses as the base 'per 1' multiplier}

  \item{identifiers}{named list of any other identifiers
  that should also be stored for this instrument}

  \item{...}{any other passthru parameters}
}
\description{
  Currency symbols (like any symbol) may be any combination
  of alphanumeric characters, but the FX market has a
  convention that says that the first currency in a
  currency pair is the 'target' and the second currency in
  the symbol pair is the currency the rate ticks in.  So
  'EURUSD' can be read as 'USD per 1 EUR'.
}
\details{
  In \code{FinancialInstrument} the \code{currency} of the
  instrument should be the currency that the spot rate
  ticks in, so it will typically be the second currency
  listed in the symbol.

  Thanks to Garrett See for helping sort out the
  inconsistencies in different naming and calculating
  conventions.
}
\references{
  http://financial-dictionary.thefreedictionary.com/Base+Currency
}

