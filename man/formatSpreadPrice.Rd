\name{formatSpreadPrice}
\alias{formatSpreadPrice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
format the price of a synthetic instrument
}
\description{
Divides the notional spread price by the spread multiplier and rounds prices to the nearest tick_size.
}
\usage{
formatSpreadPrice(x, multiplier = 1, tick_size = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
xts price series
}
  \item{multiplier}{
numeric multiplier (e.g. 1000 for crack spread to get from $ to $/bbl)
}
  \item{tick_size}{
minimum price change of the spread
}
}
\value{
price series of same length as \code{x}
}
\author{
Garrett See
}\seealso{
buildSpread, fn_SpreadBuilder
}

