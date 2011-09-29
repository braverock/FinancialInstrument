\name{month_cycle2numeric}
\alias{MC2N}
\alias{month_cycle2numeric}
\title{coerce month_cycle to a numeric vector}
\usage{
  month_cycle2numeric(month_cycle)

  MC2N(month_cycle)
}
\arguments{
  \item{month_cycle}{the expiration months of a
  \code{\link{future}}. See examples.}
}
\value{
  numeric vector
}
\description{
  This will convert month codes or month names to numeric
  months.
}
\details{
  Input can be a vector or a comma-delimited string. All
  elements of \code{month_cycle} should be similar. Do not
  mix month names, codes and numbers in the same call.
}
\examples{
month_cycle2numeric("H,M,U,Z")
month_cycle2numeric(c("H","M","U","Z"))
month_cycle2numeric("Mar,jun,SEP,dEc")
month_cycle2numeric("March,june,sep,decem")
month_cycle2numeric("3,6,9,12")
month_cycle2numeric(seq(3,12,3))
}
\author{
  Garrett See
}
\seealso{
  \code{\link{M2C}}, \code{\link{C2M}},
  \code{\link{next.future_id}} \code{\link{future}}
}

