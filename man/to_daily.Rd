\name{.to_daily}
\alias{.to_daily}
\title{Extract a single row from each day in an xts object...}
\usage{.to_daily(x, EOD_time="15:00:00")}
\description{Extract a single row from each day in an xts object}
\value{xts object with daily scale.}
\author{Garrett See}
\seealso{quantmod:::to.daily, quantmod:::to.period}
\arguments{\item{x}{xts object of sub-daily data.}
\item{EOD_time}{time of day to use.}}
