\name{synthetic.instrument}
\alias{synthetic.instrument}
\alias{synthetic}
\alias{spread}
\alias{guaranteed_spread}
\alias{butterfly}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
synthetic instrument contructors
}
\description{
define spreads, guaranteed_spreads, butterflies and other synthetic instruments
}
\usage{
synthetic.instrument(primary_id, currency, members, memberratio, ..., multiplier = 1, tick_size = NULL, identifiers = NULL, type = c("synthetic.instrument", "synthetic", "instrument"))
synthetic(primary_id , currency , multiplier=1, identifiers = NULL, ..., members=NULL, type=c("synthetic", "instrument"))
spread(primary_id, currency = NULL, members, memberratio, tick_size=NULL,
    ..., multiplier = 1, identifiers = NULL) 
guaranteed_spread(primary_id, currency, members = NULL, memberratio = c(1,-1), ..., 
    multiplier = 1, identifiers = NULL, tick_size=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{primary_id}{
chr string of primary identifier of instrument to be defined.
}
  \item{currency}{
chr string name of currency denomination
}
  \item{members}{
vector of primary_ids of member instruments
}
  \item{memberratio}{
vector of weights for each leg. negative numbers for selling.
}
  \item{\dots}{
any other passthrough parameters
}
  \item{multiplier}{
multiplier of the spread.
}
  \item{tick_size}{
minimum price change of spread
}
  \item{identifiers}{
identifiers
}
  \item{type}{
type of instrument; wrappers do not require this.
}
}
\details{
Simple derivatives like \code{\link{option}} or \code{\link{future}} contracts typically have one underlying instrument.  
While properties like strike and expiration vary for these derivative contracts or series, the underlying is well understood.

More complex derivatives are typically modeled as baskets of underlying products, and are typically traded over-the-counter or as proprietary in-house products.

The general \code{synthetic} function is intended to be extended to support these arbitrary baskets of assets.  

\code{spread} \code{guaranteed_spread} and \code{butterfly} are wrappers for \code{synthetic.instrument}. \code{synthetic.instrument} will make a call to synthetic to create the final instrument.

We welcome assistance from others to model more complex OTC derivatives such as swap products.
}
\value{
called for side effect. stores an instrument in .instrument environment
}
\author{
Brian Peterson, Garrett See
}

\seealso{
instrument, future, option_series.yahoo
}
\examples{
\dontrun{
stock('SPY','USD',1)
stock('DIA','USD',1)
spread('SPYDIA','USD',c('SPY','DIA'),c(1,-1))
}
}

