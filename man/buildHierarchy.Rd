\name{buildHierarchy}
\alias{buildHierarchy}
\title{Construct a hierarchy of instruments useful for aggregation}
\usage{
  buildHierarchy(primary_ids, levels, ...)
}
\arguments{
  \item{primary_ids}{A list of assets to be included in the
  hierarchy list}

  \item{levels}{A list of instrument attributes in top-down
  order}

  \item{...}{any other passthru parameters}
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
  fields that will be used to create 'custom' fields.
}
\author{
  Peter Carl
}

