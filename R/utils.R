#' @title Send a call to function in the server and get the result
#'
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login.
#' @param function_call a character string that represents the call to a function with its arguments.
#'
#' @return \code{callAggregationMethod} returns the result from a call to a function in the server

# Small value to avoid misleading variance value (null or negative) due to differential privacy 
# noise
delta = 0.0001

callAggregationMethod <- function(datasources, function_call) {
  res <- DSI::datashield.aggregate(datasources, as.symbol(function_call))
  return (res)
} 