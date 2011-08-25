\name{next.future_id}
\alias{next.future_id}
\alias{prev.future_id}
\title{Get the primary_id of the next-to-expire (previously expiring) future_series instrument}
\usage{
  next.future_id(id, month_cycle = seq(3, 12, 3), root =
  NULL, format = NULL)

  prev.future_id(id, month_cycle = seq(3, 12, 3), root =
  NULL, format = NULL)
}
\arguments{
  \item{id}{character string primary_id of a future_series
  instrument}

  \item{month_cycle}{months in which contracts expire.
  numeric or month codes. See Details.}

  \item{root}{root_id. usually only used if there is no
  underscore in the \code{id}. See Details.}

  \item{format}{how you would like the returned id to be
  formatted. If NULL, it will match the format of
  \code{id}. See Details.}
}
\value{
  character
}
\description{
  Using \code{\link{parse_id}}, this will figure out where
  in the \code{month_cycle} that \code{id} belongs.  Then,
  it will use the next (previous) month in
  \code{month_cycle} to construct the id of the
  next-to-expire contract.
}
\details{
  \code{month_cycle} can be a numeric vector (corresponding
  to the months in which contracts expire), or it can be a
  vector of month codes, a vector of month abbreviations,
  or a comma-delimited string of month codes or
  abbreviations, in which case an attempt will be made to
  convert it to a numeric vector. by passing it through
  \code{\link{month_cycle2numeric}}

  \code{root} is primarily used when you have an id that
  does not have an underscore, in which case, providing
  \code{root} will make splitting the id into primary_id
  and suffix_id easier and more accurate.  \code{root} can
  also be used if you want the returned id to be on a
  different \code{future} than the id you passed in (when
  used this way, \code{format} should also be used).

  By default, (when called with \code{format=NULL}) the
  returned id will be of the same format as the \code{id}
  that was passed in. The format of the returned id can be
  specified with the \code{format} argument.  See
  \code{\link{format_id}} for supported values of
  \code{format}
}
\examples{
next.future_id("ES_Z1","H,M,U,Z", format=NULL)
next.future_id("VIXAUG11", 1:12, root='VIX', format=NULL)
next.future_id("YM_Q11", seq(3,12,3)) #gives a warning about 'Q' not being part of month_cycle
}
\author{
  Garrett See
}
\seealso{
  \code{\link{format_id}} for supported values of
  \code{format}. \code{\link{month_cycle2numeric}}
}

