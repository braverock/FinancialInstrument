\name{getRoot}
\alias{getRoot}
\title{Get the root contract specs for an instrument}
\usage{
  getRoot(root_id, type = c("future", "option"))
}
\arguments{
  \item{root_id}{string (e.g. "ES", ".ES", or "..ES" for
  e-mini S&P 500 futures)}

  \item{type}{character. type of instrument to look for
  ("future" or "option"). Alternatively, can be numeric: 1
  for "future" or 2 for "option"}
}
\value{
  an object of class \code{type}
}
\description{
  Get a \code{future} or \code{option} object
}
\details{
  \code{get_option} and \code{get_future} are wrappers, and
  they are called internally by \code{\link{future_series}}
  and \code{\link{option_series}}

  \code{\link{future}} and \code{\link{option}} objects may
  have a primary_id that begins with 1 or 2 dots (in order
  to avoid naming conflics).  For example, the root specs
  for options (or futures) on the stock with ticker "SPY"
  may be stored with a primary_id of "SPY", ".SPY", or
  "..SPY"

  This function will try calling
  \code{\link{getInstrument}} using each possible
  primary_id until it finds the instrument that is of
  appropriate \code{type}.
}
\examples{
\dontrun{
option('.SPY',currency("USD"),100,underlying_id=stock("SPY","USD"))
future("..SPY","USD", 100, underlying_id="SPY")
getRoot("SPY", 'future')
getRoot("SPY", 'option')
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{getInstrument}}
}

