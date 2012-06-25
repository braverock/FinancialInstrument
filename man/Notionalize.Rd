\name{Notionalize}
\alias{Denotionalize}
\alias{Notionalize}
\title{Convert price series to/from notional value}
\usage{
  Notionalize(x, name, env = .GlobalEnv)

  Denotionalize(x, name, env = .GlobalEnv)
}
\arguments{
  \item{x}{an xts object, or an object that is coercible to
  xts}

  \item{name}{primary_id of the instrument that has the
  multiplier; usually the same as the name of \code{x}}

  \item{env}{environment. where to find \code{x} if only
  its name is provided}
}
\value{
  an object of the same class as \code{x}
}
\description{
  \code{Notionalize} multiplies all prices by the contract
  multiplier \code{Denotionalize} divides all prices by the
  contract multiplier
}
\details{
  The mulitplier is only applied to columns with prices.  A
  column is considered to be a price column if its name
  contains \dQuote{Open}, \dQuote{High}, \dQuote{Low},
  \dQuote{Close}, \dQuote{Bid}, \dQuote{Ask},
  \dQuote{Trade}, \dQuote{Mid}, or \dQuote{Price} and does
  not contain \dQuote{Size}, \dQuote{Sz}, \dQuote{Volume},
  \dQuote{Qty}, \dQuote{Quantity}, \dQuote{OpInt},
  \dQuote{OpenInterest} (not case-sensitive)
}
\examples{
\dontrun{
source("http://tinyurl.com/download-tblox")
getSymbols("CL", src='tblox')
define_futures.tblox()
tail(Notionalize(CL, "CL"))
tail(Denotionalize(Notionalize(CL), "CL"))
}
}
\author{
  Garrett See
}

