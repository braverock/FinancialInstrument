\name{ls_instruments_by}
\alias{ls_instruments_by}
\title{Subset names of instruments}
\usage{
  ls_instruments_by(what, value, in.slot = NULL, pattern =
  NULL, match = TRUE)
}
\arguments{
  \item{what}{what attribute (e.g. currency, type, strike,
  etc.)}

  \item{value}{what value must the attribute have (e.g.
  \sQuote{EUR}, \sQuote{option}, 100, etc.).  If
  \code{NULL} then the names of all instruments that have a
  \code{what} slot will be returned}

  \item{pattern}{only return instruments with
  \code{pattern} in the name}

  \item{match}{should pattern match names exactly?}

  \item{in.slot}{If the attribute you are looking for is
  stored inside another slot, this is the name of that
  slot. (usually "IB")}
}
\value{
  chr vector of instrument names
}
\description{
  list names of instruments that have an attribute that
  matches some value
}
\details{
  list instruments that have a given attribute level with a
  given value.
}
\examples{
\dontrun{
stock(c("GOOG","INTC"),currency("USD"))
synthetic("SnP","USD",src=list(name='^GSPC',src='yahoo'))
ls_instruments_by('type','stock')
ls_instruments_by("name",NULL,in.slot='src')
ls_instruments_by('src',NULL)
}
}
\author{
  Garrett See
}
\seealso{
  buildHierarchy, instrument.table, ls_instruments
}

