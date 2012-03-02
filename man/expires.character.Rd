\name{expires.character}
\alias{expires.character}
\title{character expires extraction method}
\usage{
  \method{expires}{character} (x, Date, expired = TRUE,
    silent = FALSE, ...)
}
\arguments{
  \item{Date}{Can be a Date or character string.  When
  \code{expires} is a vector, the retuned value will be one
  of the two values of \code{expires} that are closest to
  \code{Date}. (which one will be determined by the value
  of \code{expired}).}

  \item{expired}{TRUE/FALSE. This determines which date
  will be used when \code{expires} is a vector.  If
  \code{expired} is \code{TRUE} the date returned will be
  the last one before \code{Date}.  If \code{expired} is
  \code{FALSE} the first one after \code{Date} will be
  returned.}

  \item{silent}{silence warnings?}
}
\description{
  if no \code{instrument} can be found by the id of
  \code{x}, or if the \code{instrument} does not have an
  \code{expires} attribute, an attempt will be made to
  infer the year and month of expiration using
  \code{parse_id} in which case the returned value will be
  a string of the format \dQuote{YYYY-MM}.  Presently,
  \code{Date} and \code{expired} will be ignored if
  \code{x} is not the name of an instrument
}
\author{
  Garrett See
}
\seealso{
  \code{\link{expires.instrument}}
}
\keyword{internal}

