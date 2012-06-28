\name{future_series}
\alias{bond_series}
\alias{future_series}
\alias{option_series}
\title{Constructors for series contracts}
\usage{
  future_series(primary_id, root_id = NULL,
    suffix_id = NULL, first_traded = NULL, expires = NULL,
    identifiers = NULL, assign_i = TRUE, overwrite = TRUE,
    ...)

  option_series(primary_id, root_id = NULL,
    suffix_id = NULL, first_traded = NULL, expires = NULL,
    callput = c("call", "put"), strike = NULL,
    identifiers = NULL, assign_i = TRUE, overwrite = TRUE,
    ...)

  bond_series(primary_id, suffix_id, ...,
    first_traded = NULL, maturity = NULL,
    identifiers = NULL, payment_schedule = NULL,
    assign_i = TRUE)
}
\arguments{
  \item{primary_id}{String describing the unique ID for the
  instrument. May be a vector for \code{future_series} and
  \code{option_series}}

  \item{root_id}{String product code or underlying_id,
  usually something like 'ES' or 'CL' for futures, or the
  underlying stock symbol (maybe preceded with a dot) for
  equity options.}

  \item{suffix_id}{String suffix that should be associated
  with the series, usually something like 'Z9' or 'Mar10'
  denoting expiration and year.}

  \item{first_traded}{String coercible to Date for first
  trading day.}

  \item{expires}{String coercible to Date for expiration
  date}

  \item{maturity}{String coercible to Date for maturity
  date of bond series.}

  \item{callput}{Right of option; call or put}

  \item{strike}{Strike price of option}

  \item{payment_schedule}{Not currently being implemented}

  \item{identifiers}{Named list of any other identifiers
  that should also be stored for this instrument.}

  \item{assign_i}{TRUE/FALSE. Should the instrument be
  assigned in the \code{.instrument} environment?}

  \item{overwrite}{TRUE/FALSE. If FALSE, only
  \code{first_traded} and \code{expires} will be updated.}

  \item{...}{any other passthru parameters}
}
\description{
  Constructors for series contracts on instruments such as
  options and futures
}
\details{
  The root \code{instrument} (e.g. the \code{future} or
  \code{option}) must be defined first.

  In custom parameters for these series contracts, we have
  often found it useful to store attributes such as local
  roll-on and roll-off dates (rolling not on the
  \code{first_listed} or \code{expires}.

  For \code{future_series} and \code{option_series} you may
  either provide a \code{primary_id} (or vector of
  \code{primary_id}s), OR both a \code{root_id} and
  \code{suffix_id}.

  Note that the code for \code{bond} and \code{bond_series}
  has not been updated recently and may not support all the
  features supported for \code{option_series} and
  \code{future_series}.  Patches welcome.
}
\examples{
\dontrun{
currency("USD")
future("ES","USD",multiplier=50, tick_size=0.25)
future_series('ES_U1')
future_series(root_id='ES',suffix_id='Z11')
stock('SPY','USD')
option('.SPY','USD',multiplier=100,underlying_id='SPY')
#can use either .SPY or SPY for the root_id.
#it will find the one that is option specs.
option_series('SPY_110917C125', expires='2011-09-16')
option_series(root_id='SPY',suffix_id='111022P125')
option_series(root_id='.SPY',suffix_id='111119C130')
#multiple series instruments at once.
future_series(c("ES_H12","ES_M12"))
option_series(c("SPY_110917C115","SPY_110917P115"))
}
}

