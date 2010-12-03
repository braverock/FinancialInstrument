\name{synthetic.ratio}
\title{constructors for synthetic instruments...}
\usage{synthetic.ratio(primary_id, currency, multiplier=1, identifiers, ...,
    type=c("synthetic.ratio", "synthetic", "instrument"), members,
    memberratio)}
\description{constructors for synthetic instruments}
\details{Simple derivatives like \code{\link{option}} or \code{\link{future}} contracts typically have one underlying instrument.  
While properties like strike and expiration vary for these derivative contracts or series, the underlying is well understood.

More complex derivatives are typically modeled as baskets of underlying products, and are typically traded over-the-counter or as proprietary in-house products.

The general \code{synthetic} function is intended to be extended to support these arbitrary baskets of assets.  

Currently implemented examples are in ratio based spreads in \code{spread} and in exchange-guaranteed spread products in \code{guaranteed_spread}.

We welcome assistance from others to model more complex OTC derivatives such as swap products.}
\alias{synthetic}
\alias{spread}
\alias{synthetic.ratio}
\alias{guaranteed_spread}
\arguments{\item{primary_id}{string describing the unique ID for the instrument}
\item{currency}{string describing the currency ID of an object of type \code{\link{currency}}}
\item{multiplier}{numeric multiplier to apply to the price in the instrument currency to get to notional value}
\item{identifiers}{character vector of any other identifiers that should also be stored for this instrument}
\item{...}{any other passthru parameters}
\item{type}{class string, should not be set by users}
\item{members}{character vector of instrument identifiers that make up the synthetic}
\item{memberratio}{numeric vector of ratio relationships between members, e.g. c(4,3) for a 4:3 spread}}
