\name{ls_by_currency}
\alias{ls_AUD}
\alias{ls_CAD}
\alias{ls_CHF}
\alias{ls_EUR}
\alias{ls_GBP}
\alias{ls_HKD}
\alias{ls_JPY}
\alias{ls_NZD}
\alias{ls_SEK}
\alias{ls_USD}
\alias{ls_by_currency}
\alias{rm_by_currency}
\title{shows or removes instruments of given currency denomination(s)}
\usage{
  ls_by_currency(currency, pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  rm_by_currency(x, currency, keep.currencies = TRUE)

  ls_USD(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_AUD(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_GBP(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_CAD(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_EUR(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_JPY(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_CHF(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_HKD(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_SEK(pattern = NULL, match = TRUE,
    show.currencies = FALSE)

  ls_NZD(pattern = NULL, match = TRUE,
    show.currencies = FALSE)
}
\arguments{
  \item{currency}{chr vector of names of currency}

  \item{pattern}{an optional regular expression.  Only
  names matching \sQuote{pattern} are returned.}

  \item{match}{exact match?}

  \item{show.currencies}{include names of currency
  instruments in the returned names?}

  \item{keep.currencies}{Do not delete currency instruments
  when deleting multiple instruments.}

  \item{x}{what to remove. chr vector.}
}
\value{
  ls_ functions return vector of instrument names rm_
  functions return invisible / called for side-effect.
}
\description{
  ls_ functions get names of instruments denominated in a
  given currency (or currencies) rm_ functions remove
  instruments of a given currency
}
\examples{
\dontrun{
#First create instruments
currency(c('USD','CAD','GBP')
stock(c('CM','CNQ'),'CAD')
stock(c('BET','BARC'),'GBP')
stock(c('SPY','DIA'),'USD')

#now the examples
ls_by_currency(c('CAD','GBP'))

ls_USD()
ls_CAD()

#2 ways to remove all instruments of a currency
rm_instruments(ls_USD())
#rm_instruments(ls_GBP(),keep.currencies=FALSE)
rm_by_currency( ,'CAD')
#rm_by_currency( ,'CAD', keep.currencies=FALSE)
}
}
\author{
  Garrett See
}
\seealso{
  ls_instruments, ls_currencies, rm_instruments,
  rm_currencies, twsInstrument, instrument
}

