#' @title Differentially private standard deviation
#'
#' @param input_data the input vector
#' @param epsilon privacy budget
#' @param lower_bound lower bound for input values
#' @param upper_bound upper bound for input values
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.boundedStandardDeviationDP} returns a differentially private standard deviation
#' @export

ds.boundedStandardDeviationDP <- function(input_data, epsilon, lower_bound, upper_bound, datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boundedStandardDeviationDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  result <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(result)
}
