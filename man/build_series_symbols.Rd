\name{build_series_symbols}
\alias{build_series_symbols}
\title{construct a series of symbols based on root symbol and suffix letters}
\usage{
  build_series_symbols(roots, yearlist = c(0, 1))
}
\arguments{
  \item{yearlist}{vector of year suffixes to be applied,
  see Details}

  \item{roots}{data.frame containing at least columns
  \code{primary_id} and \code{month_cycle}, see Details}
}
\description{
  The columns needed by this version of the function are
  \code{primary_id} and \code{month_cycle}.
  \code{primary_id} should match the \code{primary_id} of
  the instrument describing the root contract.
  \code{month_cycle} should contain a comma delimited
  string describing the month sequence to use, e.g.
  \code{"F,G,H,J,K,M,N,Q,U,V,X,Z"} for all months using the
  standard futures letters, or \code{"H,M,U,Z"} for
  quarters, or \code{"Mar,Jun,Sep,Dec"} for quarters as
  three-letter month abbreviations, etc. The correct values
  will vary based on your data source.
}
\details{
  TODO add more flexibility in input formats for
  \code{roots} #'
}
\author{
  Brian G. Peterson
}
\seealso{
  \code{\link{load.instruments}}
}

