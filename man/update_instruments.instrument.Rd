\name{update_instruments.instrument}
\alias{update_instruments.instrument}
\title{Update instruments with metadata from another instrument.}
\usage{
  update_instruments.instrument(Symbols, source_id,
    create.new = FALSE, ignore = "identifiers",
    assign_i = TRUE)
}
\arguments{
  \item{Symbols}{charcter vector of primary_ids or other
  instrument identifiers. of instruments to be updated.
  Alternatively, \code{Symbols} can be an \code{instrument}
  or list of \code{instrument}s.}

  \item{source_id}{The primary_id (or other identifier) of
  an instrument, or an instrument.  The \code{source_id}
  instrument will be used to update the metadata of
  \code{Symbols}' instruments.}

  \item{create.new}{If FALSE (Default), only attributes
  that exist but have empty values will be updated.  If
  TRUE, new attributes will be created if \code{source_id}
  has them, but the \code{Symbols} do not.}

  \item{ignore}{vector of names of instrument attributes
  that should not be copied to the updated instruments.}

  \item{assign_i}{TRUE/FALSE. If TRUE, the updated
  instruments will be assigned back into the instrument
  environment.  If FALSE, a list of updated instruments
  will be returned}
}
\value{
  if \code{isTRUE(assign_i)} a vector of primary_ids of the
  instruments that were upated.  Otherwise, a list of
  updated instrument objects.
}
\description{
  Update instruments with metadata from another instrument.
}
\details{
  By default, only attributes that have a value of
  \code{""} will be given a new value.

  If \code{create.new} is \code{TRUE}, then if there are
  attributes in \code{source_id} that are not in the
  \code{Symbols}' instrument, those attributes will be
  copied to the updated instruments unless they are in
  \code{ignore}.
}
\note{
  one way to overwrite attributes of one instrument with
  those of another is to first set equal to \code{""} those
  attributes that you want to overwrite, then use
  \code{update_instruments.instrument} to copy the
  attributes.
}
\examples{
\dontrun{
#rm_instruments()
currency("USD")
synthetic("SPX", "USD", identifiers=list(yahoo="GSPC"),
          tick_size=0.01,
         liquidHours="T08:30:00/T15:00:00",
         extraField='something else',
         assign_i=TRUE)
stock("SPY", "USD", liquidHours="", assign_i=TRUE)
all.equal(getInstrument("SPX"), getInstrument("SPY"))
getInstrument("SPY")
## update SPY metadata based on the metadata of SPX
## Only attributes that == "" are updated by default
update_instruments.instrument("SPY", "SPX", assign_i=FALSE) #liquidHours
update_instruments.instrument("SPY", "SPX", create.new=TRUE,
                              ignore=c("identifiers", "type"),
                              assign_i=FALSE)
# Although you probably do NOT want to, this will
# copy everything new -- including identifiers and type!
update_instruments.instrument("SPY", "SPX", create.new=TRUE, ignore=NULL,
                              assign_i=FALSE)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{update_instruments.yahoo}},
  \code{\link{all.equal.instrument}}
}

