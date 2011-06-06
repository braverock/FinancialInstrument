\name{buildSpread}
\alias{buildSpread}
\title{construct a price/level series for a spread...}
\usage{buildSpread(spread_id, ..., Dates = NULL, prefer='Mid.Price', onelot=FALSE, method=c('Close','Midpoint','BA','BB'))}
\description{construct a price/level series for a spread}
\details{this function should provide a generic spread series builder.}
\seealso{\code{\link{spread}} for instructions on defining the spread}
\author{bpeterson}
\arguments{\item{spread_id}{string descrining the primary_id of an instrument of type 'spread}
\item{...}{any other passthru parameters}
\item{Dates}{date range to subset on, currently not implemented}
\item{prefer}{preferred column to use}
\item{onelot}{TRUE/FALSE, if TRUE, will divide by the number of units of the front leg to get a 'onelot'}
\item{method}{method for pricing: Close, Midpoint BA, BB} }
