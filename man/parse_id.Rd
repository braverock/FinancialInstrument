\name{parse_id}
\alias{parse_id}
\title{Parse a primary_id}
\usage{
  parse_id(x, silent = TRUE, root = NULL)
}
\arguments{
  \item{x}{the id to be parsed (e.g. \sQuote{ES_U11},
  \sQuote{SPY_111217C130})}

  \item{silent}{silence warnings?}

  \item{root}{character name of instrument root_id.
  Optionally provide this to make parsing easier.}
}
\value{
  a list of class \sQuote{id.list} containing \sQuote{root}
  and \sQuote{suffix} as well as what is returned from
  \code{\link{parse_suffix}} (type, month, year, strike,
  right, cm, cc, format)
}
\description{
  Extract/infer descriptive information about an instrument
  from its name.
}
\details{
  This function is primarily intended to be used on the
  names of \code{\link{future_series}} and
  \code{\link{option_series}} instruments, and it will work
  best if the id has an underscore in it that separates the
  root_id from the suffix_id.  (However, it should be able
  to handle most ids even if the underscore is missing).
  After splitting \code{x} into a root_id and suffix_id,
  the suffix_id is passed to \code{\link{parse_suffix}}
  (see also) for further processing.

  TODO: add support for bond_series.
}
\note{
  this function will identify \code{x} as an
  \code{\link{exchange_rate}} only if it is 6 characters
  long and made up of 2 previously defined
  \code{\link{currency}} instruments.
}
\examples{
parse_id("ES_Z11")
parse_id("CLZ1")
parse_id("SPY_111217C130")
}
\author{
  Garrett See
}
\seealso{
  \code{\link{parse_suffix}}
}

