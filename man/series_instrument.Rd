\name{future_series}
\alias{bond_series}
\alias{future_series}
\alias{option_series}
\title{constructors for series contracts on instruments such as options and futures}
\usage{
  future_series(primary_id, suffix_id, first_traded = NULL,
  expires = NULL, identifiers = NULL, ...)

  option_series(primary_id, suffix_id, first_traded = NULL,
  expires = NULL, callput = c("call", "put"), identifiers =
  NULL, ...)

  bond_series(primary_id, suffix_id, ..., first_traded =
  NULL, maturity = NULL, identifiers = NULL,
  payment_schedule = NULL)
}
\arguments{
  \item{primary_id}{string describing the unique ID for the
  instrument}

  \item{suffix_id}{string suffix that should be associated
  with the series, usually something like 'Z9' or 'Mar10'
  denoting expiration and year}

  \item{first_traded}{string coercible to Date for first
  trading day}

  \item{expires}{string coercible to Date for expiration
  date}

  \item{maturity}{string coercible to Date for maturity
  date of bond series}

  \item{callput}{right of option; call or put}

  \item{payment_schedule}{not currently being implemented}

  \item{identifiers}{named list of any other identifiers
  that should also be stored for this instrument}

  \item{...}{any other passthru parameters}
}
\description{
  In custom parameters for these series contracts, we have
  often found it useful to store attributes such as local
  roll-on and roll-off dates (rolling not on the
  \code{first_listed} or \code{expires}
}

