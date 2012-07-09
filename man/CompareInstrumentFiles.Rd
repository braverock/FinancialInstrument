\name{CompareInstrumentFiles}
\alias{CompareInstrumentFiles}
\title{Compare Instrument Files}
\usage{
  CompareInstrumentFiles(file1, file2, ...)
}
\arguments{
  \item{file1}{A file containing an instrument environment}

  \item{file2}{Another file containing an instrument
  environment}

  \item{...}{Arguments to pass to
  \code{\link{all.equal.instrument}}}
}
\value{
  A list that contains the names of all instruments that
  were added, the names of all instruments that were
  removed, and the changes to all instruments that were
  updated (per \code{\link{all.equal.instrument}}).
}
\description{
  Compare the .instrument environments of two files
}
\details{
  This will load two instrument files (created by
  \code{\link{saveInstruments}}) and find the differences
  between them.  In addition to returning a list of
  difference that are found, it will produce messages
  indicating the number of instruments that were added, the
  number of instruments that were removed, and the number
  of instruments that are different.
}
\examples{
\dontrun{
#backup current .instrument environment
bak <- as.list(FinancialInstrument:::.instrument, all.names=TRUE)
old.wd <- getwd()
tmpdir <- tempdir()
setwd(tmpdir)
rm_instruments(keep=FALSE)
# create some instruments and save
stock(c("SPY", "DIA", "GLD"), currency("USD"))
saveInstruments("MyInstruments1")
# make some changes
rm_stocks("GLD")
stock("QQQ", "USD")
instrument_attr("SPY", "description", "S&P ETF")
saveInstruments("MyInstruments2")
CompareInstrumentFiles("MyInstruments1", "MyInstruments2")
#Clean up
setwd(old.wd)
reloadInstruments(bak)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{saveInstruments}},
  \code{\link{all.equal.instrument}}
}

