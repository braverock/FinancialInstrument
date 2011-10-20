\name{ls_underlyings}
\alias{ls_underlyings}
\title{show names of underlyings}
\usage{
  ls_underlyings(pattern = NULL, match = TRUE)
}
\arguments{
  \item{pattern}{an optional regular expression.  Only
  names matching \sQuote{pattern} are returned.}

  \item{match}{require exact match?}
}
\value{
  chr vector of names of unique \code{underlying_id}s
}
\description{
  shows names that are stored in the \code{underlying_id}
  slot of derivative instruments
}
\details{
  first calls \code{ls_derivatives}, then looks for unique
  \code{underlying_id}s. If no derivatives have been
  defined, nothing will be returned.
}
\examples{
\dontrun{
ls_underlyings()
}
}
\author{
  Garrett See
}
\seealso{
  ls_instruments_by, ls_derivatives, ls_options, ls_futures
}

