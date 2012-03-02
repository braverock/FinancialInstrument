\name{expires.xts}
\alias{expires.xts}
\title{xts expires extraction method}
\usage{
  \method{expires}{xts} (x, Date, expired = TRUE,
    silent = FALSE, src = c("data", "instrument"), ...)
}
\arguments{
  \item{src}{either \dQuote{data} or \dQuote{instrument}.}
}
\value{
  If \code{src} is \dQuote{data}, the returned value will
  be the index of the last price that is not \code{NA}
  (price is determined by \code{quantmod:::getPrice}.
  \code{getPrice} arguments \code{symbol} and \code{prefer}
  can be passed through dots.)

  If \code{src} is \dQuote{instrument} the symbol of the
  xts object will be passed to
  \code{\link{expires.character}}
}
\description{
  determines (or estimates) the expiration from an xts
  object by either finding the last row that is not
  \code{NA} or by passing the name/symbol of the xts object
  to \code{\link{expires.character}}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{expires.instrument}}, \code{\link{expires}}
}
\keyword{internal}

