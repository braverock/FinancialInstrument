\name{getInstrument}
\alias{getInstrument}
\title{primary accessor function for getting objects of type 'instrument'}
\usage{
  getInstrument(x, Dates = NULL, silent = FALSE)
}
\arguments{
  \item{x}{string identifier of instrument to retrieve}

  \item{Dates}{date range to retrieve 'as of', may not
  currently be implemented}

  \item{silent}{if TRUE, will not warn on failure, default
  FALSE}
}
\description{
  This function will search the \code{.instrument}
  environment, using first the \code{primary_id} and then
  any \code{identifiers} to locate the instrument.
}

