\name{ls_instruments}
\alias{ls_bonds}
\alias{ls_calls}
\alias{ls_currencies}
\alias{ls_derivatives}
\alias{ls_exchange_rates}
\alias{ls_funds}
\alias{ls_futures}
\alias{ls_future_series}
\alias{ls_FX}
\alias{ls_guaranteed_spreads}
\alias{ls_instruments}
\alias{ls_non_currencies}
\alias{ls_non_derivatives}
\alias{ls_options}
\alias{ls_option_series}
\alias{ls_puts}
\alias{ls_spreads}
\alias{ls_stocks}
\alias{ls_synthetics}
\alias{rm_bonds}
\alias{rm_currencies}
\alias{rm_derivatives}
\alias{rm_exchange_rates}
\alias{rm_funds}
\alias{rm_futures}
\alias{rm_future_series}
\alias{rm_FX}
\alias{rm_instruments}
\alias{rm_non_derivatives}
\alias{rm_options}
\alias{rm_option_series}
\alias{rm_spreads}
\alias{rm_stocks}
\alias{rm_synthetics}
\title{List or Remove instrument objects}
\usage{
  ls_instruments(pattern = NULL, match = TRUE,
    verbose = TRUE)

  ls_stocks(pattern = NULL, match = TRUE)

  ls_options(pattern = NULL, match = TRUE,
    include.series = TRUE)

  ls_option_series(pattern = NULL, match = TRUE)

  ls_futures(pattern = NULL, match = TRUE,
    include.series = TRUE)

  ls_future_series(pattern = NULL, match = TRUE)

  ls_currencies(pattern = NULL, match = TRUE)

  ls_non_currencies(pattern = NULL, includeFX = TRUE,
    match = TRUE)

  ls_exchange_rates(pattern = NULL, match = TRUE)

  ls_FX(pattern = NULL, match = TRUE)

  ls_bonds(pattern = NULL, match = TRUE)

  ls_funds(pattern = NULL, match = TRUE)

  ls_spreads(pattern = NULL, match = TRUE)

  ls_guaranteed_spreads(pattern = NULL, match = TRUE)

  ls_synthetics(pattern = NULL, match = TRUE)

  ls_derivatives(pattern = NULL, match = TRUE)

  ls_non_derivatives(pattern = NULL, match = TRUE)

  ls_calls(pattern = NULL, match = TRUE)

  ls_puts(pattern = NULL, match = TRUE)

  rm_instruments(x, keep.currencies = TRUE)

  rm_stocks(x)

  rm_options(x)

  rm_option_series(x)

  rm_futures(x)

  rm_future_series(x)

  rm_currencies(x)

  rm_exchange_rates(x)

  rm_bonds(x)

  rm_funds(x)

  rm_spreads(x)

  rm_synthetics(x)

  rm_derivatives(x)

  rm_non_derivatives(x, keep.currencies = TRUE)
}
\arguments{
  \item{pattern}{an optional regular expression.  Only
  names matching \sQuote{pattern} are returned.}

  \item{match}{return only exact matches?}

  \item{verbose}{be verbose?}

  \item{include.series}{should future_series or
  option_series instruments be included.}

  \item{x}{what to remove. if not supplied all instruments
  of relevent class will be removed.  For
  \code{ls_defined.by} x is the string describing how the
  instrument was defined.}

  \item{keep.currencies}{If TRUE, currencies will not be
  deleted.}

  \item{includeFX}{should exchange_rates be included in
  ls_non_currencies results}
}
\value{
  ls functions return vector of character strings
  corresponding to instruments of requested type rm
  functions are called for side-effect
}
\description{
  display the names of or delete instruments, stocks,
  options, futures, currencies, bonds, funds, spreads,
  guaranteed_spreads, synthetics, derivatives, or
  non-derivatives.
}
\details{
  ls functions return the names of all the instruments of
  the class implied by the function name. rm functions
  remove the instruments of the class implied by the
  function name

  rm_instruments and rm_non_derivatives will not delete
  currencies unless the keep.currencies argument is FALSE.

  For the rm functions, x can be a vector of instrument
  names, or nothing.  If \code{x} is missing, all
  instruments of the relevant type will be removed.

  It can be useful to nest these functions to get things
  like futures denominated in USD.
}
\examples{
\dontrun{
#rm_instruments(keep.currencies=FALSE) #remove everything from .instrument

# First, create some instruments
currency(c("USD", "EUR", "JPY"))
#stocks
stock(c("S", "SE", "SEE", "SPY"), 'USD')
synthetic("SPX", "USD", src=list(src='yahoo', name='^GSPC'))
#derivatives
option('.SPY', 'USD', multiplier=100, underlying_id='SPY')
option_series(root_id="SPY", expires='2011-06-18', callput='put', strike=130)
option_series(root_id="SPY", expires='2011-09-17', callput='put', strike=130)
option_series(root_id="SPY", expires='2011-06-18', callput='call', strike=130)
future('ES', 'USD', multiplier=50, expires='2011-09-16', underlying_id="SPX")
option('.ES','USD',multiplier=1, expires='2011-06',strike=1350, right='C', underlying_id='ES')

# Now, the examples
ls_instruments() #all instruments
ls_instruments("SE") #only the one stock
ls_instruments("S", match=FALSE) #anything with "S" in name

ls_currencies()
ls_stocks()
ls_options()
ls_futures()
ls_derivatives()
ls_puts()
ls_non_derivatives()
#ls_by_expiry('20110618',ls_puts()) #put options that expire on Jun 18th, 2011
#ls_puts(ls_by_expiry('20110618')) #same thing

rm_options('SPY_110618C130')
rm_futures()
ls_instruments()
#rm_instruments('EUR') #Incorrect
rm_instruments('EUR', keep.currencies=FALSE) #remove the currency
rm_currencies('JPY') #or remove currency like this
ls_currencies()
ls_instruments()

rm_instruments() #remove all but currencies
rm_currencies()

option_series.yahoo('DIA')
ls_instruments_by('underlying_id','DIA') #underlying_id must exactly match 'DIA'
ls_derivatives('DIA',match=FALSE) #primary_ids that contain 'DIA'
rm_instruments()
}
}
\author{
  Garrett See
}
\seealso{
  ls_instruments_by, ls_by_currency, ls_by_expiry, ls, rm,
  instrument, stock, future, option, currency,
  FinancialInstrument::sort_ids
}

