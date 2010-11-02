\name{instrument}
\title{instrument class constructors...}
\usage{instrument(primary_id, ..., currency, multiplier, tick_size,
    identifiers, type)}
\description{instrument class constructors}
\details{All 'currency' instruments must be defined before instruments of other types may be defined.

In \dots you may pass any other arbitrary instrument fields.  
S3 classes in \R are basically lists with a class attribute.
We use this to our advantage to allow us to set arbitrary fields.

\code{identifiers} should be a named list to specify other identifiers beyond the \code{primary_id}.
Please note that whenever possible, these should still be unique.  Perhaps Bloomberg, Reuters-RIC, CUSIP, etc.  
The code will return the first (and only the first) match that it finds, starting with the primary_id, and then searching all instruments in the list alphabetically by primary_id.  
This is robust enough if you take some care, though a more robust patch would be welcomed.}
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
\item{identifiers}{named list of any other identifiers that should also be stored for this instrument}
\item{type}{instrument type to be appended to the class definition, typically not set by user}
\item{underlying_id}{for derivatives, the identifier of the instrument that this one is derived from, may be NULL for cash settled instruments}}
