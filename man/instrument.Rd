\name{instrument}
\alias{bond}
\alias{currency}
\alias{fund}
\alias{future}
\alias{instrument}
\alias{option}
\alias{stock}
\title{instrument class constructors}
\usage{
  instrument(primary_id, ..., currency, multiplier,
    tick_size = NULL, identifiers = NULL, type = NULL,
    assign_i = FALSE, overwrite = TRUE)

  stock(primary_id, currency = NULL, multiplier = 1,
    tick_size = 0.01, identifiers = NULL, assign_i = TRUE,
    overwrite = TRUE, ...)

  fund(primary_id, currency = NULL, multiplier = 1,
    tick_size = 0.01, identifiers = NULL, assign_i = TRUE,
    overwrite = TRUE, ...)

  future(primary_id, currency, multiplier,
    tick_size = NULL, identifiers = NULL, assign_i = TRUE,
    overwrite = TRUE, ..., underlying_id = NULL)

  option(primary_id, currency, multiplier,
    tick_size = NULL, identifiers = NULL, assign_i = TRUE,
    overwrite = TRUE, ..., underlying_id = NULL)

  currency(primary_id, identifiers = NULL, assign_i = TRUE,
    ...)

  bond(primary_id, currency, multiplier, tick_size = NULL,
    identifiers = NULL, assign_i = TRUE, overwrite = TRUE,
    ...)
}
\arguments{
  \item{primary_id}{String describing the unique ID for the
  instrument. Most of the wrappers allow this to be a
  vector.}

  \item{...}{Any other passthru parameters, including}

  \item{underlying_id}{For derivatives, the identifier of
  the instrument that this one is derived from, may be
  \code{NULL} for cash settled instruments}

  \item{currency}{String describing the currency ID of an
  object of type \code{\link{currency}}}

  \item{multiplier}{Numeric multiplier to apply to the
  price in the instrument to get to notional value.}

  \item{tick_size}{The tick increment of the instrument
  price in it's trading venue, as numeric quantity (e.g.
  1/8 is .125)}

  \item{identifiers}{Named list of any other identifiers
  that should also be stored for this instrument}

  \item{type}{instrument type to be appended to the class
  definition, typically not set by user}

  \item{assign_i}{TRUE/FALSE. Should the instrument be
  assigned to the \code{.instrument} environment?  Default
  is FALSE for \code{instrument}, TRUE for wrappers.}

  \item{overwrite}{TRUE/FALSE. Should existing instruments
  with the same primary_id be overwritten? Default is TRUE.
  If FALSE, an error will be thrown and the instrument will
  not be created.}
}
\description{
  All 'currency' instruments must be defined before
  instruments of other types may be defined.
}
\details{
  In \dots you may pass any other arbitrary instrument
  fields that will be used to create 'custom' fields.  S3
  classes in \R are basically lists with a class attribute.
  We use this to our advantage to allow us to set arbitrary
  fields.

  \code{identifiers} should be a named list to specify
  other identifiers beyond the \code{primary_id}.  Please
  note that whenever possible, these should still be
  unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
  \code{\link{getInstrument}} will return the first (and
  only the first) match that it finds, starting with the
  primary_id, and then searching the primary_ids of all
  instruments for each of the \code{identifiers}.  Note
  that when a large number of instruments are defined, it
  is faster to find instruments by \code{primary_id} than
  by \code{identifiers} because it looks for
  \code{primary_id}s first.

  The \code{primary_id} will be coerced within reason to a
  valid \R variable name by using \code{\link{make.names}}.
  We also remove any leading '1' digit (a simple workaround
  to account for issues with the Reuters API).  If you are
  defining an instrument that is not a \code{currency},
  with a primary_id that already belongs to a
  \code{currency}, a new primary_id will be create using
  \code{make.names}.  For example, \code{stock("USD",
  currency("USD"))}, would create a stock with a primary_id
  of \dQuote{USD.1} instead of overwritting the
  \code{currency}.

  Please use some care to choose your primary identifiers
  so that R won't complain.  If you have better regular
  expression code, we'd be happy to include it.

  Identifiers will also try to be discovered as regular
  named arguments passed in via \code{...}.  We currently
  match any of the following:
  \code{"CUSIP","SEDOL","ISIN","OSI","Bloomberg","Reuters","X.RIC","CQG","TT","Yahoo","Google"}
  Others may be specified using a named list of
  identifiers, as described above.

  \code{assign_i} will use \code{\link{assign}} to place
  the constructed instrument class object into the
  \code{.instrument} environment.  Most of the special
  type-specific constructors will use \code{assign_i=TRUE}
  internally. Calling with \code{assign_i=FALSE}, or not
  specifying it, will return an object and will \emph{not}
  store it.  Use this option ether to wrap calls to
  \code{instrument} prior to further processing (and
  presumably assignment) or to test your parameters before
  assignment.

  If \code{overwrite=FALSE} is used, an error will be
  thrown if any \code{primary_id}s are already in use.

  As of version 0.10.0, the .instrument environment is
  located at the top level of the package. i.e.
  \code{.instrument}.

  \code{future} and \code{option} are used to define the
  contract specs of a series of instruments.  The
  \code{primary_id} for these can begin with 1 or 2 dots if
  you need to avoid overwriting another instrument. For
  example, if you have a \code{stock} with \sQuote{SPY} as
  the \code{primary_id}, you could use \sQuote{.SPY} as the
  \code{primary_id} of the \code{option} specs, and
  \sQuote{..SPY} as the \code{primary_id} of the single
  stock \code{future} specs. (or vice versa)

  You can (optionally) provide a \code{src} argument in
  which case, it will be used in a call to
  \code{\link[quantmod]{setSymbolLookup}}.
}
\seealso{
  \code{\link{currency}}, \code{\link{exchange_rate}},
  \code{\link{option_series}}, \code{\link{future_series}},
  \code{\link{spread}}, \code{\link{load.instruments}}
}

