#' Right align on interval
#' 
#' Right-align on a regular interval. Include the aligned interval in the alignment. 
#' 
#' @param timestamps POSIX time stamps
#' @param alignment regular alignment in seconds
#' @export
rightAlignOnInterval <- function(timestamps, alignment){
  aligns <- unclass(timestamps) %% alignment
  
  timestamps + ifelse(aligns == 0, 0, alignment) - aligns
}