\name{load.instruments}
\alias{load.instruments}
\title{load instrument metadata into the...}
\usage{load.instruments(file, ..., metadata, id_col=1, default_type="stock")}
\description{load instrument metadata into the .instrument environment}
\details{This function will load instrument metadata (data about the data)
either from a file specified by the \code{file} argument or
from a \code{data.frame} specified by the \code{metadata} argument.

The function will attempt to make reasonable assumptions about what you're trying to do, but this isn't magic.

You will typically need to specify the \code{type} of instrument to be loaded, failure to do so will generate a Warning and \code{default_type} will be used.

You will need to specify a \code{primary_id}, or define a \code{id_col} that contains the data to be used as the primary_id of the instrument.

You will need to specify a \code{currency}, unless the instrument \code{type} is 'currency'

Typically, columns will exist for \code{multiplier} and \code{tick_size}.

Any other columns necessary to define the specified instrument type will also be required to avoid fatal Errors.  

Additional columns will be processed, either as additional identifiers for recognized identifier names, or as custom fields.  See \code{\link{instrument}} for more information on custom fields.}
\seealso{instrument}
\arguments{\item{file}{string identifying file to load, default NULL, see Details}
\item{...}{any other passthru parameters}
\item{metadata}{optional, data.frame containing metadata, default NULL, see Details}
\item{id_col}{numeric column containing id if primary_id isn't defined, default 1}
\item{default_type}{character string to use as instrument type fallback, see Details}}
