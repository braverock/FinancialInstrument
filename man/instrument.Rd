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
  assign_i = FALSE)

  stock(primary_id, currency = NULL, multiplier = 1,
  tick_size = 0.01, identifiers = NULL, ...)

  fund(primary_id, currency = NULL, multiplier = 1,
  tick_size = 0.01, identifiers = NULL, ...)

  future(primary_id, currency, multiplier, tick_size =
  NULL, identifiers = NULL, ..., underlying_id = NULL)

  option(primary_id, currency, multiplier, tick_size =
  NULL, identifiers = NULL, ..., underlying_id = NULL)

  currency(primary_id, currency = NULL, multiplier = 1,
  identifiers = NULL, ...)

  bond(primary_id, currency, multiplier, tick_size = NULL,
  identifiers = NULL, ...)
}
\arguments{
  \item{primary_id}{string describing the unique ID for the
  instrument}

  \item{...}{any other passthru parameters, including}

  \item{underlying_id}{for derivatives, the identifier of
  the instrument that this one is derived from, may be NULL
  for cash settled instruments}

  \item{currency}{string describing the currency ID of an
  object of type \code{\link{currency}}}

  \item{multiplier}{numeric multiplier to apply to the
  price in the instrument currency to get to notional
  value}

  \item{tick_size}{the tick increment of the instrument
  price in it's trading venue, as numeric quantity (e.g.
  1/8 is .125)}

  \item{identifiers}{named list of any other identifiers
  that should also be stored for this instrument}

  \item{type}{instrument type to be appended to the class
  definition, typically not set by user}

  \item{assign_i}{TRUE/FALSE if TRUE, assign the instrument
  to the .instrument environment, default FALSE}
}
\description{
  All 'currency' instruments must be defined before
  instruments of other types may be defined.
}
\details{
  In \dots you may pass any other arbitrary instrument
  fields that will be used to create 'custom' fields. S3
  classes in \R are basically lists with a class attribute.
  We use this to our advantage to allow us to set arbitrary
  fields.

  \code{identifiers} should be a named list to specify
  other identifiers beyond the \code{primary_id}. Please
  note that whenever possible, these should still be
  unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
  The code will return the first (and only the first) match
  that it finds, starting with the primary_id, and then
  searching all instruments in the list alphabetically by
  primary_id. This is robust enough if you take some care,
  though a more robust patch would be welcomed.

  The \code{primary_id} will be coerced within reason to a
  valid \R variable name by using \code{\link{make.names}}.
  We also remove any leading '1' digit (a simple workaround
  to account for issues with the Reuters API). Please use
  some care to choose your primary identifiers so that R
  won't complain. If you have better regular expression
  code, we'd be happy to include it.

  Identifiers will also try to be discovered as regular
  named arguments passed in via \code{...}. We currently
  match any of the following:
  \code{"CUSIP","SEDOL","ISIN","OSI","Bloomberg","Reuters","X.RIC","CQG","TT","Yahoo","Google"}
  Others mat be specified using a named list of
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

  \code{future} and \code{option} are used to define the
  contract specs of a series of instruments.  The
  \code{primary_id} for these can begin with 1 or 2 dots if
  you need to avoid overwriting another instrument. For
  example, if you have a \code{stock} with \sQuote{SPY} as
  the \code{primary_id}, you could use \sQuote{.SPY} as the
  \code{primary_id} of the \code{option} specs, and
  \sQuote{..SPY} as the \code{primary_id} of the single
  stock \code{future} specs. (or vice versa)
}
\seealso{
  \code{\link{exchange_rate}} \code{\link{option_series}}
  \code{\link{future_series}} \code{\link{spread}}
  \code{\link{load.instruments}}
}

