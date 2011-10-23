\name{ls_by_expiry}
\alias{ls_by_expiry}
\alias{rm_by_expiry}
\title{list or remove instruments by expiration date}
\usage{
  ls_by_expiry(expiry, pattern = NULL, match = TRUE)

  rm_by_expiry(x, expiry)
}
\arguments{
  \item{expiry}{expiration date that should correspond to
  the \sQuote{expires} field of an instrument}

  \item{pattern}{an optional regular expression.  Only
  names matching \sQuote{pattern} are returned.}

  \item{match}{exact match of pattern?}

  \item{x}{what to remove}
}
\value{
  \code{ls_by_expiry} gives a vector of names of
  instruments that expire on the given expiry.
  \code{rm_by_expiry} is called for its side-effect.
}
\description{
  show names of or remove instruments that expire on a
  given date
}
\details{
  \code{ls_by_expiry} will find instruments that have a
  field named either \dQuote{expiry} or \dQuote{expires}
  with a value that matches \code{expiry}.
}
\examples{
\dontrun{
ls_by_expiry('20110917')
ls_by_expiry('20110917',ls_options())
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{ls_instruments}}, \code{\link{ls_options}},
  \code{\link{ls_calls}}, \code{\link{ls_puts}},
  \code{\link{ls_futures}}, \code{\link{ls_derivatives}}
}

