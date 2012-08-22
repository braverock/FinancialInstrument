\name{synthetic}
\alias{butterfly}
\alias{guaranteed_spread}
\alias{ICS}
\alias{ICS_root}
\alias{spread}
\alias{synthetic}
\alias{synthetic.instrument}
\title{synthetic instrument constructors}
\usage{
  synthetic(primary_id = NULL, currency = NULL,
    multiplier = 1, identifiers = NULL, assign_i = TRUE,
    ..., members = NULL, type = "synthetic")

  synthetic.instrument(primary_id, currency, members,
    memberratio, ..., multiplier = 1, tick_size = NULL,
    identifiers = NULL, assign_i = TRUE,
    type = c("synthetic.instrument", "synthetic"))

  spread(primary_id = NULL, currency = NULL, members,
    memberratio, tick_size = NULL, ..., multiplier = 1,
    identifiers = NULL, assign_i = TRUE)

  butterfly(primary_id = NULL, currency = NULL, members,
    tick_size = NULL, identifiers = NULL, assign_i = TRUE,
    ...)

  guaranteed_spread(primary_id = NULL, currency = NULL,
    root_id = NULL, suffix_id = NULL, members = NULL,
    memberratio = c(1, -1), ..., multiplier = NULL,
    identifiers = NULL, assign_i = TRUE, tick_size = NULL)

  ICS_root(primary_id, currency = NULL, members,
    multiplier = NULL, identifiers = NULL, assign_i = TRUE,
    tick_size = NULL, ...)

  ICS(primary_id, assign_i = TRUE, identifiers = NULL, ...)
}
\arguments{
  \item{primary_id}{chr string of primary identifier of
  instrument to be defined.}

  \item{currency}{chr string name of currency denomination}

  \item{members}{vector of primary_ids of member
  instruments}

  \item{memberratio}{vector of weights for each leg.
  negative numbers for selling.}

  \item{\dots}{any other passthrough parameters}

  \item{multiplier}{multiplier of the spread (1 / divisor
  for price weighted baskets)}

  \item{tick_size}{minimum price change of the spread}

  \item{identifiers}{identifiers}

  \item{assign_i}{TRUE/FALSE. Should the instrument be
  assigned in the \code{.instrument} environment?}

  \item{type}{type of instrument; wrappers do not require
  this.}

  \item{root_id}{instrument identifier for the root
  contract, default NULL}

  \item{suffix_id}{identifiers for the member contract
  suffixes, default NULL, will be split as \code{members},
  see Details}
}
\value{
  called for side effect. stores an instrument in
  .instrument environment
}
\description{
  define spreads, guaranteed_spreads, butterflies, and
  other synthetic instruments
}
\details{
  Simple derivatives like \code{\link{option}} or
  \code{\link{future}} contracts typically have one
  underlying instrument. While properties like strike and
  expiration vary for these derivative contracts or series,
  the underlying is well understood.

  More complex derivatives are typically modeled as baskets
  of underlying products, and are typically traded
  over-the-counter or as proprietary in-house products.

  The general \code{synthetic} function is intended to be
  extended to support these arbitrary baskets of assets.

  \code{spread} \code{guaranteed_spread} and
  \code{butterfly} are wrappers for
  \code{synthetic.instrument}. \code{synthetic.instrument}
  will make a call to synthetic to create the final
  instrument.

  The \code{suffix_id} parameter of wrapper functions such
  as \code{guaranteed_spread} is presumed to be a string
  describing the \code{members}. It will be
  \code{\link{strsplit}} using the regex "[-;:_,\\.]" to
  create the \code{members} vector, and potentially
  combined with a \code{root_id}.

  Most wrappers will build \code{primary_id} if it is NULL,
  either by combining \code{root_id} and \code{suffix_id},
  or by passing \code{members} in a call to
  \code{\link{make_spread_id}}

  \code{ICS} will build an Intercommodity Spread.  Although
  the expiration date and ratio may change, the members of
  a given ICS will not change.  Therefore, \code{ICS_root}
  can be used to hold the members of an Intercommodity
  Spread.  If an \code{ICS_root} has not been defined, then
  \code{members} will be a required argument for \code{ICS}

  We welcome assistance from others to model more complex
  OTC derivatives such as swap products.
}
\examples{
\dontrun{
stock('SPY','USD',1)
stock('DIA','USD',1)
spread('SPY.DIA','USD',c('SPY','DIA'),c(1,-1))
}
}
\author{
  Brian Peterson, Garrett See
}
\seealso{
  instrument, future, option_series.yahoo
}

