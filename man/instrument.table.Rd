\name{instrument.table}
\alias{instrument.table}
\title{Create data.frame with attributes of all instruments}
\usage{
  instrument.table(symbols = NULL, exclude = NULL,
    attrs.of = NULL)
}
\arguments{
  \item{symbols}{A vector of instrument names to include}

  \item{exclude}{A vector of names of attributes that
  should not be included in the returned data.frame}

  \item{attrs.of}{name of a FinancialInstrument instrument.
  Returned data.frame columns will be the attributes of
  instrument.}
}
\value{
  data.frame
}
\description{
  A wrapper for \code{\link{buildHierarchy}}, that defaults
  to returning all attributes. By default it looks for the
  instrument with the most attribute levels, and uses those
  attributes for columns.  If you would prefer to use the
  attribute levels of a given instrument to build the
  columns, use \code{attrs.of}.
}
\details{
  if there are some attributes that you do not want to be
  included in the returned data.frame, specify them with
  \code{exclude}.
}
\examples{
\dontrun{
currency('USD')
stock('GM','USD',exchange='NYSE')
stock('XOM','USD',description='Exxon Mobil')
instrument.table()
#Usually, currencies will not have as many attribute levels
#as other instruments, so you may want to exclude them from the table.
it <- instrument.table(exclude="USD|GM", attrs.of = "XOM") #columns created based on XOM instrument
#it <- instrument.table(exclude=c('USD','GM'), attrs.of = "XOM") #same thing
it <- instrument.table(exclude='tick_size|description|exchange')
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{buildHierarchy}}, \code{\link{instrument}}
}

