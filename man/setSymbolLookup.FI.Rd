\name{setSymbolLookup.FI}
\alias{setSymbolLookup.FI}
\title{set quantmod-style SymbolLookup for instruments}
\usage{
  setSymbolLookup.FI(base_dir, ..., split_method =
  c("days", "common"), storage_method = "rda",
  use_identifier = "primary_id", extension = "rda", src =
  "FI")
}
\arguments{
  \item{base_dir}{string specifying the base directory
  where data is stored, see Details}

  \item{\dots}{any other passthru parameters}

  \item{storage_method}{currently only \sQuote{rda}, but we
  will eventually support \sQuote{indexing} at least, and
  maybe others}

  \item{split_method}{string specifying the method files
  are split, currently \sQuote{days} or \sQuote{common},
  see Details}

  \item{use_identifier}{string identifying which column
  should be use to construct the \code{primary_id} of the
  instrument, default 'primary_id'}

  \item{extension}{file extension, default "rda"}

  \item{src}{which \code{\link[quantmod]{getSymbols}}
  sub-type to use, default \code{\link{getSymbols.FI}} by
  setting 'FI'}
}
\description{
  This function exists to tell
  \code{\link[quantmod]{getSymbols}} where to look for your
  repository of market data.
}
\details{
  The \code{base_dir} parameter \emph{must} be set or the
  function will fail. This will vary by your local
  environment and operating system.  For mixed-OS
  environments, we recommend doing some OS-detection and
  setting the network share to your data to a common
  location by operating system.  For example, all Windows
  machines may use \dQuote{M:/} and all *nix-style (linux,
  Mac) machines may use \dQuote{/mnt/mktdata/}.

  The \code{split_method} currently allows either
  \sQuote{days} or \sQuote{common}, and expects the file or
  files to be in sub-directories named for the symbol.  In
  high frequency data, it is standard practice to split the
  data by days, which is why that option is the default.
}
\seealso{
  \code{\link{load.instruments}}
  \code{\link{getSymbols.FI}}
  \code{\link{load.instruments}}
  \code{\link[quantmod]{setSymbolLookup}}
}

