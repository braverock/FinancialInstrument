\name{synthetic.ratio}
\title{constructors for synthetic instruments...}
\usage{synthetic.ratio(primary_id, currency, multiplier=1, identifiers, ..., cl=c("synthetic.ratio", "synthetic", "instrument"), members, memberratio)}
\description{constructors for synthetic instruments}
\alias{synthetic}
\alias{spread}
\alias{synthetic.ratio}
\arguments{\item{primary_id}{string describing the unique ID for the instrument}
\item{currency}{string describing the currency ID of an object of type \code{\link{currency}}}
\item{multiplier}{numeric multiplier to apply to the price in the instrument currency to get to notional value}
\item{identifiers}{character vector of any other identifiers that should also be stored for this instrument}
\item{...}{any other passthru parameters}
\item{cl}{class string, should not be set by users}
\item{members}{character vector of instrument identifiers that make up the synthetic}
\item{memberratio}{numeric vector of ratio relationchips between members, e.g. c(4,3) for a 4:3 spread}}
