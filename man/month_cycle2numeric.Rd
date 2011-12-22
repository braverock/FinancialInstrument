\name{month_cycle2numeric}
\alias{MC2N}
\alias{month_cycle2numeric}
\title{coerce month_cycle to a numeric vector}
\usage{
  month_cycle2numeric(...)

  MC2N(...)
}
\arguments{
  \item{...}{the expiration months of a
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
  Input can be a vector, comma-delimited string, or
  multiple strings. All inputs should be similar. Do not
  mix month names, codes and numbers in the same call.

  \code{MC2N} is an alias
}
\examples{
MC2N("H,M,U,Z") # from single string
MC2N(c("H","M","U","Z")) # from single vector
MC2N("h", "M", "u", "Z") # from multiple strings
MC2N(c("F","G"), "H", c("X","Z")) # from multiple vectors
month_cycle2numeric("Mar,jun,SEP,dEc")
month_cycle2numeric("Mar", "jun", "SEP", "dEc")
MC2N("March,june,sep,decem")
MC2N("March, june, sep, decem") #spaces between commas are ok
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

