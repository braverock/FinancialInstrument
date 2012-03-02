\name{expires.instrument}
\alias{expires.instrument}
\title{instrument expires extraction method}
\usage{
  \method{expires}{instrument} (x, Date, expired = TRUE,
    silent = FALSE, ...)
}
\arguments{
  \item{Date}{Can be a Date or character string.  When
  \code{expires} is a vector, the retuned value will be one
  of the two values of \code{expires} that are closest to
  \code{Date}. (which one will be determined by the value
  of \code{expired})}

  \item{expired}{TRUE/FALSE. This determines which date
  will be used when \code{expires} is a vector.  If
  \code{expired} is \code{TRUE} the date returned will be
  the last one before \code{Date}.  If \code{expired} is
  \code{FALSE} the first one after \code{Date} will be
  returned. Note that if \code{expires} is a single value,
  \code{expired} will be ignored.}

  \item{silent}{silence warnings?}
}
\description{
  Returns either the last expiration date before
  \code{Date}, or the first expiration date after
  \code{Date} (if \code{expired==FALSE}).
}
\author{
  Garrett See
}
\seealso{
  \code{\link{expires}}
}
\keyword{internal}

