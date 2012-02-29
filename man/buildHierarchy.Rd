\name{buildHierarchy}
\alias{buildHierarchy}
\title{Construct a hierarchy of instruments useful for aggregation}
\usage{
  buildHierarchy(primary_ids, levels, ...)
}
\arguments{
  \item{primary_ids}{A character vector of
  \code{instrument} primary_ids to be included in the
  hierarchy list}

  \item{levels}{A character vector of instrument attributes
  in top-down order}

  \item{...}{not in use}
}
\value{
  Constructs a data.frame that contains the list of assets
  in the first column and the category or factor for
  grouping at each level in the following columns
}
\description{
  All 'currency' instruments must be defined before
  instruments of other types may be defined.
}
\details{
  In \dots you may pass any other arbitrary instrument
  fields that will be used to create 'custom' fields. (This
  is not yet implemented)
}
\author{
  Peter Carl, Alexis Petit, Garrett See
}
\seealso{
  \code{\link{instrument.table}}
}

