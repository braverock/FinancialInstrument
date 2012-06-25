\name{getSymbols.FIpar}
\alias{getSymbols.FIpar}
\title{getSymbols method for loading data from split files}
\usage{
  getSymbols.FIpar(Symbols, from = "2010-01-01",
    to = Sys.Date(), ..., dir = "", return.class = "xts",
    extension = "rda", split_method = c("days", "common"),
    use_identifier = NA, date_format = NULL,
    verbose = TRUE, days_to_omit = c("Saturday", "Sunday"),
    indexTZ, timespan)
}
\arguments{
  \item{Symbols}{a character vector specifying the names of
  each symbol to be loaded}

  \item{from}{Retrieve data no earlier than this date.
  Default '2010-01-01'.}

  \item{to}{Retrieve data through this date. Default
  Sys.Date().}

  \item{...}{any other passthru parameters}

  \item{dir}{if not specified in getSymbolLookup, directory
  string to use.  default ""}

  \item{return.class}{only "xts" is currently supported}

  \item{extension}{file extension, default "rda"}

  \item{split_method}{string specifying the method used to
  split the files, currently \sQuote{days} or
  \sQuote{common}, see
  \code{\link[FinancialInstrument]{setSymbolLookup.FI}}}

  \item{use_identifier}{optional. identifier used to
  construct the \code{primary_id} of the instrument.  If
  you use this, you must have previously defined the
  \code{\link[FinancialInstrument]{instrument}}}

  \item{date_format}{format as per the
  \code{\link{strptime}}, see Details}

  \item{verbose}{TRUE/FALSE}

  \item{days_to_omit}{character vector of names of weekdays
  that should not be loaded.  Default is
  \code{c("Saturday", "Sunday")}.  Use \code{NULL} to
  attempt to load data for all days of the week.}

  \item{indexTZ}{valid TZ string. (e.g.
  \dQuote{America/Chicago} or \dQuote{America/New_York})
  See \code{\link[xts]{indexTZ}}.}

  \item{timespan}{xts-style intraday subsetting string.
  e.g. \dQuote{T08:30:00/T15:00:00}.  If \code{indexTZ} is
  provided, timezone conversion will occur before the time
  of day subsetting. \code{timespan} is only used if
  \code{split_method} is \dQuote{days}.}
}
\description{
  This is \code{\link[FinancialInstrument]{getSymbols.FI}}
  adapted to use \code{foreach}.  If a parallel backend has
  been registered (with, e.g. \code{doMC:::registerDoMC} or
  \code{doSMP:::registerDoSMP},) it will get multiple
  symbols in parallel.
}
\details{
  Meant to be called internally by
  \code{\link[quantmod]{getSymbols}} .

  If date_format is NULL (the Default), we will assume an
  ISO date as changed by \code{\link{make.names}}, for
  example, 2010-12-01 would be assumed to be a file
  containing 2010.12.01

  If auto.assign is FALSE, you'll only get the last Symbol
  in the symbols list returned, so only use
  \code{auto.assign=FALSE} with a single symbol.  Patches
  Welcome.
}
\seealso{
  \code{\link[quantmod]{getSymbols}}
  \code{\link{getSymbols.FI}}
  \code{\link[FinancialInstrument]{saveSymbols.days}}
  \code{\link[FinancialInstrument]{instrument}}
  \code{\link[FinancialInstrument]{setSymbolLookup.FI}}
  \code{\link[FinancialInstrument]{load.instruments}}
  \code{\link[quantmod]{getSymbols}}
}

