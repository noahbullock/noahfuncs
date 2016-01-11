\name{apply.periodly}
\alias{apply.periodly}
\title{Apply by period}
\usage{
  apply.periodly(x, FUN, period, k = 1, ...)
}
\arguments{
  \item{x}{an \code{xts} or possibly \code{zoo} object over
  which to apply a function}

  \item{FUN}{a function to apply}

  \item{period}{the period by which to apply the function}

  \item{k}{the number of periods in each aggregation}

  \item{...}{optional arguments to \code{FUN}}
}
\description{
  Apply a function by a standard periodicity. A more
  flexible version of the '\code{apply.*ly}' family of
  functions in \code{xts}.
}
\details{
  Valid values for the argument on include: \code{"us"
  (microseconds), "microseconds", "ms" (milliseconds),
  "milliseconds", "secs" (seconds), "seconds", "mins"
  (minutes), "minutes", "hours", "days", "weeks", "months",
  "quarters", and "years"}.
}

