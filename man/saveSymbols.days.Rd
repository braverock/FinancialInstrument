\name{saveSymbols.days}
\alias{saveSymbols.common}
\alias{saveSymbols.days}
\title{save data to disk}
\usage{
  saveSymbols.days(Symbols, base_dir = "", env =
  .GlobalEnv)

  saveSymbols.common(Symbols, base_dir = "", env =
  .GlobalEnv)
}
\arguments{
  \item{Symbols}{vector of character names of objects to be
  saved}

  \item{base_dir}{character.  directory in which to store
  data.}

  \item{env}{environment that holds the data to be saved
  (.GlobalEnv by default)}
}
\value{
  called for side-effect.
}
\description{
  save data to disk the way that \code{getSymbols.FI}
  expects it to be saved.
}
\examples{
\dontrun{
getSymbols("SPY", src='yahoo')
saveSymbols.common("SPY", base_dir="~/")
rm("SPY")
getSymbols("SPY", src='FI', dir="~/", split_method='common')
unlink("~/SPY", recursive=TRUE)
}
}
\seealso{
  \code{\link{getSymbols.FI}}
}

