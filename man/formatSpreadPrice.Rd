\name{formatSpreadPrice}
\alias{formatSpreadPrice}
\title{format the price of a synthetic instrument}
\usage{
  formatSpreadPrice(x, multiplier = 1, tick_size = 0.01)
}
\arguments{
  \item{x}{xts price series}

  \item{multiplier}{numeric multiplier (e.g. 1000 for crack
  spread to get from $ to $/bbl)}

  \item{tick_size}{minimum price change of the spread}
}
\value{
  price series of same length as \code{x}
}
\description{
  Divides the notional spread price by the spread
  multiplier and rounds prices to the nearest
  \code{tick_size}.
}
\author{
  Garrett See
}
\seealso{
  \code{\link{buildSpread}}, \code{\link{fn_SpreadBuilder}}
}

