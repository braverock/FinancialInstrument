\name{build_spread_symbols}
\alias{build_spread_symbols}
\title{build symbols for exchange guaranteed (calendar) spreads}
\usage{
  build_spread_symbols(data = NULL, file = NULL, outputfile
  = NULL, start_date = Sys.Date())
}
\arguments{
  \item{data}{data.frame containing at least columns
  \code{primary_id}, \code{month_cycle}, amd
  \code{contracts_ahead}, see Details}

  \item{file}{if not NULL, will read input data from the
  file named by this argument, in the same folrmat as
  \code{data}, above}

  \item{outputfile}{if not NULL, will write out put to this
  file as a CSV}

  \item{start_date}{date to start building from, of type
  \code{Date} or an ISO-8601 date string, defaults to
  \code{\link{Sys.Date}}}
}
\description{
  The columns needed by this version of the function are
  \code{primary_id}, \code{month_cycle}, and code
  \code{contracts_ahead}.
}
\details{
  \code{primary_id} should match the \code{primary_id} of
  the instrument describing the root contract.

  \code{month_cycle} should contain a comma delimited
  string describing the month sequence to use, e.g.
  \code{"F,G,H,J,K,M,N,Q,U,V,X,Z"} for all months using the
  standard futures letters, or \code{"H,M,U,Z"} for
  quarters, or \code{"Mar,Jun,Sep,Dec"} for quarters as
  three-letter month abbreviations, etc. The correct values
  will vary based on your data source.

  \code{contracts_ahead} should contain a comma-delimited
  string describing the cycle on which the guaranteed
  calendar spreads are to be consructed, e.g. '1' for
  one-month spreads, '1,3' for one and three month spreads,
  '1,6,12' for 1, 6, and 12 month spreads, etc.

  \code{active_months} is a numeric field indicating how
  many months including the month of the \code{start_date}
  the contract is available to trade. This number will be
  used as the upper limit for symbol generation.

  One of \code{data} or \code{file} must be populated for
  input data.
}
\author{
  Ilya Kipnis <Ilya.Kipnis<at>gmail.com>
}
\seealso{
  \code{\link{load.instruments}}
  \code{\link{build_series_symbols}}
}

