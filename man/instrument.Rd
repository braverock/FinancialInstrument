\name{instrument}
\title{instrument class constructors...}
\usage{instrument(primary_id, ..., currency, multiplier, tick_size,
    identifiers, type)}
\description{instrument class constructors}
\details{All 'currency' instruments must be defined before instruments of other types may be defined}
\alias{stock}
\alias{bond}
\alias{future}
\alias{option}
\alias{currency}
\alias{instrument}
\seealso{\code{\link{exchange_rate}}
\code{\link{option_series}}
\code{\link{future_series}}}
\arguments{\item{primary_id}{string describing the unique ID for the instrument}
\item{...}{any other passthru parameters}
\item{currency}{string describing the currency ID of an object of type \code{\link{currency}}}
\item{multiplier}{numeric multiplier to apply to the price in the instrument currency to get to notional value}
\item{tick_size}{the tick increment of the instrument price in it's trading venue, as numeric quantity (e.g. 1/8 is .125)}
\item{identifiers}{character vector of any other identifiers that should also be stored for this instrument}
\item{type}{instrument type to be appended to the class definition}
\item{underlying_id}{for derivatives, the identifier of the instrument that this one is derived from, may be NULL for cash settled instruments}}
