\name{expires.spread}
\alias{expires.spread}
\title{spread expires extraction method}
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
}
\description{
  \code{x$expires} will be returned if it is not
  \code{NULL}.  Otherwise, the (character representation of
  the) exiration date of the first-to-expire of the
  \code{members} will be returned.
}
\author{
  Garrett See
}
\seealso{
  \code{\link{expires.instrument}}
}
\keyword{internal}

