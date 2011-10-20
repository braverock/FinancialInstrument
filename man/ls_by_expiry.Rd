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
  the sQuoteexpires slot of an instrument}

  \item{pattern}{an optional regular expression.  Only
  names matching sQuotepattern are returned.}

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
  ls_instruments, ls_options, ls_calls, ls_puts,
  ls_futures, ls_derivatives
}

