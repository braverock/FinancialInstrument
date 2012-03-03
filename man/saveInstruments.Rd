\name{saveInstruments}
\alias{loadInstruments}
\alias{saveInstruments}
\title{Save and Load all instrument definitions}
\usage{
  saveInstruments(file_name = "MyInstruments", dir = "")

  loadInstruments(file_name = "MyInstruments", dir = "")
}
\arguments{
  \item{file_name}{What to name the file (name of file that
  holds a .instrument enviroment) Does not include file
  extension.}

  \item{dir}{Directory of file (defaults to current working
  directory. ie. "")}
}
\value{
  Called for side-effect
}
\description{
  Saves (loads) the .instrument environment to (from) disk.
}
\details{
  After you have defined some instruments, you can use
  \code{saveInstruments} to save the entire .instrument
  environment to disk.

  \code{loadInstruments} will read a file that contains
  instruments and add those instrument definitions to your
  .instrument environment

  The \code{file_name} should have a file extension of
  \dQuote{RData}, \dQuote{rda}, \dQuote{R}, or
  \dQuote{txt}.  If the \code{file_name} does not end with
  one of those, \dQuote{.RData} will be appended to the
  \code{file_name}

  If the file extension is \dQuote{R} or \dQuote{txt},
  \code{saveInstruments} will create a text file of \R code
  that can be \code{\link{source}}d to load instruments
  back into the .instrument environment.
}
\examples{
\dontrun{
stock("SPY", currency("USD"), 1)
tmpdir <- tempdir()
saveInstruments("MyInstruments.RData", dir=tmpdir)
rm_instruments(keep.currencies=FALSE)
loadInstruments("MyInstruments.RData", dir=tmpdir)
# write .R file that can be sourced
saveInstruments("MyInstruments.R", dir=tmpdir)
rm_instruments(keep.currencies=FALSE)
loadInstruments("MyInstruments.R", dir=tmpdir)
#source(file=paste(tmpdir, "MyInstruments.R", sep="/")) # same
unlink(tmpdir, recursive=TRUE)
}
}
\author{
  Garrett See
}
\seealso{
  save, load load.instrument define_stocks, define_futures,
  define_options (option_series.yahoo)
}

