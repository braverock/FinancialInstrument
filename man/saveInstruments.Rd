\name{saveInstruments}
\alias{loadInstruments}
\alias{saveInstruments}
\title{Save and Load all instrument definitions}
\usage{
  saveInstruments(file_name = "MyInstruments", dir = "",
  extension = "RData")

  loadInstruments(file_name = "MyInstruments", dir = "",
  extension = "RData", env = .GlobalEnv)
}
\arguments{
  \item{file_name}{What to name the file (name of file that
  holds a .instrument enviroment) Does not include file
  extension.}

  \item{dir}{Directory of file (defaults to current working
  directory. ie. "")}

  \item{extension}{File extension of file. default is
  RData}

  \item{env}{What environment holds .instrument environment
  to be updated; usually .GlobalEnv.}
}
\value{
  Called for side-effect
}
\description{
  Saves (loads) the .instrument environment to (from) disk.
  \code{loadInstruments} will add instrument definitions
  that were saved to your .instrument environment
}
\details{
  After you have defined some instruments, you can use
  \code{saveInstruments} to save the entire .instrument
  environment to disk.
}
\examples{
\dontrun{
stock("SPY","USD",1)
saveInstruments()
loadInstruments()
}
}
\author{
  Garrett See
}
\seealso{
  save, load load.instrument define_stocks, define_futures,
  define_options (option_series.yahoo)
}

