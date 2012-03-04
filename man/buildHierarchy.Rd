\name{buildHierarchy}
\alias{buildHierarchy}
\title{Construct a hierarchy of instruments useful for aggregation}
\usage{
  buildHierarchy(primary_ids, ...)
}
\arguments{
  \item{primary_ids}{A character vector of
  \code{instrument} primary_ids to be included in the
  hierarchy list}

  \item{...}{character names of instrument attributes in
  top-down order.}
}
\value{
  Constructs a data.frame that contains the list of assets
  in the first column and the category or factor for
  grouping at each level in the following columns
}
\description{
  Construct a hierarchy of instruments useful for
  aggregation
}
\examples{
\dontrun{
# rm_instruments(keep.currencies=FALSE)
## Define some stocks
update_instruments.TTR(c("XOM", "IBM", "CVX", "WMT", "GE"), exchange="NYSE")

buildHierarchy(ls_instruments(), "type")
buildHierarchy(ls_stocks(), c("Name", "Sector"))
buildHierarchy(ls_stocks(), "Industry", "MarketCap")
}
}
\author{
  Peter Carl, Alexis Petit, Garrett See
}
\seealso{
  \code{\link{instrument.table}}
}

