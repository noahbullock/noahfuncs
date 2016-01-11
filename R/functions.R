#' Week of month
#' 
#' Get the week of the month - starting on Sunday - given a generally recognized time/Date/etc. format. 
#' 
#' @param x a vector of times/Dates, etc
#' @return a vector of the week of month for each x
#' @export
#' @importFrom lubridate mday wday
#' @examples
#' dates <- Sys.Date() + 1:30
#' data.frame(dates = dates, wom = wom(dates))
wom <- function(x){
  x <- as.Date(x, tz=tz(x))
  x <- ceiling((mday(x) + (7-wday(x)))/7)
  return(x)
}

#' Apply by period
#' 
#' Apply a function by a standard periodicity. A more flexible version of the '\code{apply.*ly}' family of functions in \code{xts}.
#' 
#' @param x an \code{xts} or possibly \code{zoo} object over which to apply a function
#' @param FUN a function to apply
#' @param period the period by which to apply the function
#' @param k the number of periods in each aggregation
#' @param ... optional arguments to \code{FUN}
#' @details Valid values for the argument on include: \code{"us" (microseconds), "microseconds", "ms" (milliseconds), "milliseconds", "secs" (seconds), "seconds", "mins" (minutes), "minutes", "hours", "days", "weeks", "months", "quarters", and "years"}.
#' @export
#' @importFrom xts endpoints period.apply
apply.periodly <- function (x, FUN, period, k=1, ...) 
{
  ep <- endpoints(x, on=period, k=k)
  period.apply(x, ep, FUN, ...)
}

#' Random string generator
#' 
#' Generate random strings, as for a password
#' 
#' @param size the string length
#' @param times the number of strings to create
#' @param set the set of possible values to use
#' @export
#' @examples
#' set.seed(40)
#' rstring(size=20, times=4)
#' set.seed(sample(2^31, size=1))
rstring <- function(size = 14, times = 1, set = c(LETTERS, letters, 0:9)){
  sapply(1:times, FUN=function(x) paste0(sample(x=set, size=size, replace=TRUE), collapse=""), USE.NAMES=FALSE)
}

#' Noah's preferred options
#' 
#' Set options, system variables, etc. as I like them
#' 
#' @export
#' @examples
#' Sys.getenv("TZ")
#' getOption("stringsAsFactors")
#' 
#' NO()
#' 
#' Sys.getenv("TZ")
#' getOption("stringsAsFactors")
NO <- function(){
  Sys.setenv(TZ = "America/New_York")
  options(stringsAsFactors=FALSE)
}

#' divide the value by the rolling mean of the value
#' 
#' this is a simplified way of removing the trend component, prior to performing subsequent analysis, like HMM
#' 
#' @param xts.obj a one-column xts object
#' @param window a window over which to apply the rolling function
#' @return an xts object
#' @importFrom zoo as.zoo rollapply
#' @importFrom xts as.xts xts
#' @export
valueOverRollingMean <- function(xts.obj, window=365){
  Zval <- as.zoo(xts.obj)
  rollM <- as.xts(rollapply(data=Zval, width=window, FUN=mean, align="center", partial = TRUE, na.rm = T))
  
  Rover <- xts.obj / rollM
  return(Rover)
}
