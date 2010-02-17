\name{buildSpread}
\alias{buildSpread}
\title{construct a price/level series for a spread...}
\usage{buildSpread(spread_id, ..., Dates, onelot=FALSE)}
\description{construct a price/level series for a spread}
\details{this function should provide a generic spread series builder.}
\seealso{\code{\link{spread}} for instructions on defining the spread}
\author{bpeterson}
\arguments{\item{spread_id}{string descrining the primary_id of an instrument of type 'spread}
\item{...}{any other passthru parameters}
\item{Dates}{date range to subset on, currently not implemented}
\item{onelot}{TRUE/FALSE, if TRUE, will divide by the number of units of the front leg to get a 'onelot'}}
