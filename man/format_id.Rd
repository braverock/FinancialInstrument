\name{format_id}
\alias{format_id}
\title{format an id}
\usage{
  format_id(id, format = NULL, parse = c("id", "suffix"),
    sep = "_", ...)
}
\arguments{
  \item{id}{character. the id to be reformatted. Can be
  either a primary_id or a suffix_id}

  \item{format}{character string indicating how the id
  should be formatted. See Details.}

  \item{parse}{character name of parsing method to use:
  "id" or "suffix"}

  \item{sep}{character that will separate root_id and
  suffix_id of output if calling with \code{parse="id"}}

  \item{...}{parameters to pass to the parsing function}
}
\value{
  character id of the appropriate format
}
\description{
  convert the primary_id or suffix_id of an
  \code{instrument} to a different format. Primarily
  intended for \code{\link{future_series}} instruments.
}
\details{
  Formats for the suffix_id include 'CY', 'CYY', and
  'CYYYY' where C is the month code and Y is numeric.
  'MMMY', 'MMMYY', 'MMMYYYY' where MMM is an uppercase
  month abbreviation. '1xCY', '1xCYY', '1xCYYYY' for
  single-stock-futures.

  There are currently only 2 formats available for
  \code{\link{option_series}}: 'opt2' and 'opt4' where opt2
  uses a 2 digit year and opt4 uses a 4 digit year.
}
\examples{
format_id('U1', format='MMMYY', parse='suffix')
format_id('ES_JUN2011', format='CYY', parse='id')
format_id("SPY_20110826P129","opt2")
#several at once
id3 <- c('VX_aug1','ES_U1', 'VX_U11')
format_id(id3,'MMMYY')
format_id(id3,'CYY')
format_id(id3,'CY',sep="")
}
\author{
  Garrett See
}
\seealso{
  \code{\link{parse_id}}, \code{\link{parse_suffix}},
  \code{\link{M2C}}, \code{\link{month_cycle2numeric}}
}

