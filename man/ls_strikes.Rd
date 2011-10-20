\name{ls_strikes}
\alias{ls_strikes}
\title{show strike prices of defined options}
\usage{
  ls_strikes(pattern = NULL)
}
\arguments{
  \item{pattern}{an optional regular expression. Only names
  matching 'pattern' are returned.}
}
\value{
  vector of strike prices
}
\description{
  list the strike prices of previously defined options.
}
\details{
  If no option names are supplied, the strike prices of all
  defined options will be returned
}
\examples{
\dontrun{
option_series.yahoo('SPY')
ls_strikes(ls_options('SPY'))
}
}
\author{
  Garrett See
}
\seealso{
  ls_options, ls_calls, ls_puts ls_instruments_by
  ls_underlyings
}

