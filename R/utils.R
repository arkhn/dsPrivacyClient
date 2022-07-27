# #' @title Launch a call to function in the server and get the result
# #'
# #' @param datasources a list of \code{\link{DSConnection-class}} 
# #' objects obtained after login. If the \code{datasources} argument is not specified
# #' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
# #' @param function_call a character string that represents the call to a function with its arguments.
# #'
# #' @return \code{getAggregation} returns the result from a call to a function in the server

getAggregation <- function(datasources, function_call) {
  res <- DSI::datashield.aggregate(datasources, as.symbol(function_call))
  return (res)
} 