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
\examples{
ids <- c("ES_U11",'GLD','SPY',"YM_Jun11",'DIA','VX_V10')
sort_ids(ids)
}
\author{
  Garrett See
}
\seealso{
  \code{\link{parse_id}}
}

