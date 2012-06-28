\name{make_spread_id}
\alias{make_spread_id}
\title{Construct a primary_id for a \code{spread} \code{instrument} from the
primary_ids of its members}
\usage{
  make_spread_id(x, root = NULL, format = NULL, sep = "_")
}
\arguments{
  \item{x}{character vector of member primary_ids}

  \item{root}{Optional character string of root_id to use.}

  \item{format}{String indicating how to format the
  suffix_ids of the spread.  If \code{NULL} (the default),
  or \code{FALSE}, no formatting will be done.  See
  \code{\link{format_id}} for other accepted values for
  \code{format}}

  \item{sep}{character string to separate root_id and
  suffix_id}
}
\value{
  character string that can be used as a primary_id for a
  \code{\link{spread}} instrument
}
\description{
  Construct a primary_id for a \code{spread}
  \code{instrument} from the primary_ids of its members
}
\examples{
ids <- c('VX_aug1','VX_U11')
make_spread_id(ids, format='CY')
make_spread_id(ids, format=FALSE)
make_spread_id(c("VIX_JAN11","VIX_FEB11"),root='VX',format='CY')
}
\author{
  Garrett See
}
\seealso{
  \code{\link{spread}}, \code{\link{build_spread_symbols}},
  \code{\link{build_series_symbols}}
}

