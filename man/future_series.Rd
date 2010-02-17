\name{future_series}
\title{constructors for series contracts on instruments such as options and futures...}
\usage{future_series(primary_id, suffix_id, first_traded, expires, identifiers, ...)}
\usage{option_series(primary_id, suffix_id, first_traded, expires, callput=c("call", "put"), identifiers, ...)}
\description{constructors for series contracts on instruments such as options and futures}
\alias{option_series}
\alias{future_series}
\arguments{\item{primary_id}{string describing the unique ID for the instrument}
\item{suffix_id}{string suffix that should be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year}
\item{first_traded}{string coercible to Date for first trading day}
\item{expires}{string coercible to Date for expiration date}
\item{identifiers}{character vector of any other identifiers that should also be stored for this instrument}
\item{...}{any other passthru parameters}}
