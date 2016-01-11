\name{kWhPerDay}
\alias{kWhPerDay}
\title{kWh per Day}
\usage{
  kWhPerDay(x, missing.action)
}
\arguments{
  \item{x}{an \code{xts} object to aggregate}

  \item{missing.action}{should missing hours be estimated}
}
\description{
  Aggregate sub-hourly kW data to total kWh per period and
  change \code{index} to \code{Date}.
}

