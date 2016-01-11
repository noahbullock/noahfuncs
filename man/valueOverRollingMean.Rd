\name{valueOverRollingMean}
\alias{valueOverRollingMean}
\title{divide the value by the rolling mean of the value}
\usage{
  valueOverRollingMean(xts.obj, window = 365)
}
\arguments{
  \item{xts.obj}{a one-column xts object}

  \item{window}{a window over which to apply the rolling
  function}
}
\value{
  an xts object
}
\description{
  this is a simplified way of removing the trend component,
  prior to performing subsequent analysis, like HMM
}

