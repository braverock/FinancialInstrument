\name{future_series}
\title{constructors for series contracts on instruments such as options and futures...}
\usage{future_series(primary_id, suffix_id, first_traded, expires, identifiers, ...)}
\description{constructors for series contracts on instruments such as options and futures}
\details{In custom parameters for these series contracts, we have often found it
useful to store attributes such as local roll-on and roll-off dates
(rolling not on the \code{first_listed} or \code{expires}}
\alias{option_series}
\alias{future_series}
\alias{bond_series}
\arguments{\item{primary_id}{string describing the unique ID for the instrument}
\item{suffix_id}{string suffix that should be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year}
\item{first_traded}{string coercible to Date for first trading day}
\item{expires}{string coercible to Date for expiration date}
\item{maturity}{string coercible to Date for maturity date of bond series}
\item{identifiers}{named list of any other identifiers that should also be stored for this instrument}
\item{...}{any other passthru parameters}}
