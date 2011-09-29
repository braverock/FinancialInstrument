\name{C2M}
\alias{C2M}
\alias{M2C}
\title{Month-to-Code and Code-to-Month}
\usage{
  C2M(code)

  M2C(month)
}
\arguments{
  \item{code}{Month code: F, G, H, J, K, M, N , Q, U, V, X,
  or Z}

  \item{month}{Abbreviated month: jan, feb, mar, apr, may,
  jun, jul, aug, sep, oct, nov, or dec}
}
\value{
  corresponding code or month.
}
\description{
  Convert month code (used for futures contracts) to
  abbreviated month name, or convert abbreviated month name
  to month code
}
\examples{
C2M()
C2M("M")
C2M()[6]
M2C()
M2C("Sep")
M2C()[9]
}
\author{
  Garrett See
}
\seealso{
  \code{\link{MC2N}}
}

