\name{instrument.auto}
\alias{instrument.auto}
\title{Create an instrument based on name alone}
\usage{
  instrument.auto(primary_id, currency = "USD", multiplier
  = 1, silent = FALSE, default_type = "NULL", ...)
}
\arguments{
  \item{primary_id}{charater primary identifier of
  instrument to be created}

  \item{currency}{character name of currency that
  instrument will be denominated it. Default=\dQuote{USD}}

  \item{multiplier}{numeric product multiplier}

  \item{silent}{TRUE/FALSE. silence warnings?}

  \item{default_type}{What type of instrument to make if it
  is not clear from the primary_id. ("stock", "future",
  etc.)}

  \item{...}{other passthrough parameters}
}
\value{
  Primarily called for its side-effect, but will return the
  name of the instrument that was created
}
\description{
  Given a name, this function will attempt to create an
  instrument of the appropriate type.
}
\note{
  If \code{currency} is not already defined, it will be
  defined (unless it is not 3 uppercase characters). The
  default value for \code{currency} is \dQuote{USD}.  If
  you do not provide a value for \code{currency},
  \dQuote{USD} will be defined and used to create the
  instrument.

  This is not intended to be used to create instruments of
  type \code{stock}, \code{future}, \code{option}, or
  \code{bond} although it may be updated in the future.
}
\examples{
\dontrun{
instrument.auto("CL_H1.U1")
getInstrument("CL_H1.U1") #guaranteed_spread

instrument.auto("ES_H1.YM_H1")
getInstrument("ES_H1.YM_H1") #synthetic

currency(c("USD","EUR"))
instrument.auto("EURUSD")
getInstrument("EURUSD") #made an exchange_rate

instrument.auto("VX_H11") #no root future defined yet!
getInstrument("VX_H11") #couldn't find future, didnt make future_series
future("VX","USD",1000,underlying_id=synthetic("SPX","USD")) #make the root
instrument.auto("VX_H11") #and try again
getInstrument("VX_H11") #made a future_series
}
}
\author{
  Garrett See
}

