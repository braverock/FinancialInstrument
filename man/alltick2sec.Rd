\name{alltick2sec}
\alias{alltick2sec}
\title{Convert several files from tick to 1 second}
\usage{
  alltick2sec(getdir = "~/TRTH/tick/",
    savedir = "~/TRTH/sec/", Symbols = list.files(getdir),
    overwrite = FALSE)
}
\arguments{
  \item{getdir}{directory to get tick data from}

  \item{savedir}{directory to save converted data in}

  \item{Symbols}{names of instruments to convert}

  \item{overwrite}{TRUE/FALSE. If file already exists in
  savedir, should it be overwritten?}
}
\value{
  list of files that were converted
}
\description{
  Convert several files from tick to 1 second
}
\examples{
\dontrun{
convert.log <- alltick2sec()
}
}
\author{
  gsee
}

