\name{saveSymbols.days}
\alias{saveSymbols.common}
\alias{saveSymbols.days}
\title{save data to disk}
\usage{
  saveSymbols.days(Symbols, base_dir = "", extension =
  "rda", env = .GlobalEnv)

  saveSymbols.common(Symbols, base_dir = "", extension =
  "rda", env = .GlobalEnv)
}
\arguments{
  \item{Symbols}{vector of character names of objects to be
  saved}

  \item{base_dir}{character.  directory in which to store
  data.}

  \item{extension}{file extension (\dQuote{rda})}

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
\details{
  If they do not already exist, subdirectories will be
  created for each of the \code{Symbols}.
  \code{saveSymbols.common} will save a single \sQuote{rda}
  file for each of the \code{Symbols} in that symbol's
  subdirectory. \code{saveSymbols.days} will split the data
  up into days and save a separate \sQuote{rda} file for
  each day in that symbol's subdirectory.
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

