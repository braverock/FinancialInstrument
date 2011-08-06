\name{instrument_attr}
\alias{instrument_attr}
\title{add or change an attribute of an instrument}
\usage{
  instrument_attr(primary_id, attr, value)
}
\arguments{
  \item{primary_id}{primary_id of the instrument that will
  be updated}

  \item{attr}{name of the slot that will be added or
  changed}

  \item{value}{what to assign to the \code{attr} slot of
  the \code{primary_id} instrument}
}
\value{
  called for side-effect
}
\description{
  This function will add or overwrite the data stored in
  the specified slot of the specified instrument.
}
\note{
  you can remove an attribute/slot from an instrument by
  calling this function with \code{value=NULL}
}
\examples{
\dontrun{
currency("USD")
stock("SPY","USD")
instrument_attr("USD","description","U.S. Dollar")
instrument_attr("SPY", "description", "An ETF")
getInstrument("USD")
getInstrument("SPY")

#Call with value=NULL to remove an attribute
instrument_attr("SPY", "description", NULL)
getInstrument("SPY")
}
}

