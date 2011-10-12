\name{getSymbols.FI}
\alias{getSymbols.FI}
\title{getSymbols method for loading data from split files}
\usage{
  getSymbols.FI(Symbols, from = "2010-01-01", to =
  Sys.Date(), ..., env, dir = "", return.class = "xts",
  extension = "rda", split_method = c("days", "common"),
  use_identifier, date_format = NULL, verbose = TRUE,
  auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{a character vector specifying the names of
  each symbol to be loaded}

  \item{from}{Retrieve data no earlier than this date.
  Default '2010-01-01'.}

  \item{to}{Retrieve data through this date. Default
  Sys.Date().}

  \item{...}{any other passthru parameters}

  \item{env}{where to create objects. Default .GlobalEnv}

  \item{dir}{if not specified in getSymbolLookup, directory
  string to use.  default ""}

  \item{return.class}{only "xts" is currently supported}

  \item{extension}{file extension, default "rda"}

  \item{split_method}{string specifying the method used to
  split the files, currently \sQuote{days} or
  \sQuote{common}, see \code{\link{setSymbolLookup.FI}}}

  \item{use_identifier}{optional. identifier used to
  construct the \code{primary_id} of the instrument. If you
  use this, you must have previously defined the
  \code{\link{instrument}}}

  \item{date_format}{format as per the
  \code{\link{strptime}}, see Details}

  \item{verbose}{TRUE/FALSE}

  \item{auto.assign}{TRUE/FALSE}
}
\description{
  This function should probably get folded back into
  getSymbols.rda in quantmod.
}
\details{
  Meant to be called internally by
  \code{\link[quantmod]{getSymbols}} .

  The symbol lookup table will most likely be loaded by
  \code{\link{setSymbolLookup.FI}}

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
  \code{\link{instrument}} \code{\link{setSymbolLookup.FI}}
  \code{\link{load.instruments}}
  \code{\link[quantmod]{getSymbols}}
}

