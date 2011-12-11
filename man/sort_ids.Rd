\name{sort_ids}
\alias{sort_ids}
\title{sort primary_ids of instruments}
\usage{
  sort_ids(ids, ...)
}
\arguments{
  \item{ids}{character vector of ids}

  \item{...}{arguments to pass through to
  \code{\link{parse_id}}}
}
\value{
  sorted character vector of the same length as \code{ids}
}
\description{
  Primarily intended for use on the primary_ids of
  \code{\link{future_series}} instruments. This will sort
  ids by expiration.  All ids that do not contain month and
  year information will be sorted alphabetically
  (separately) and appended to the end of the other sorted
  ids.
}
\details{
  If an instrument is defined, and has a date in its
  \sQuote{expires} field, that date will be used as the
  expiration date.  Otherwise, it is assumed that the
  contract expires on the first day of its expiration
  month.  This means that if some products are defined and
  other products that expire in the same month are not
  defined, the ones that are not defined will come first in
  the vector of sorted ids.
}
\examples{
\dontrun{
ids <- c("ES_U11",'GLD','SPY',"YM_Jun11",'DIA','VX_V10')
sort_ids(ids)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{parse_id}}
}

