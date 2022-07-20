#' @title Differentially private min
#'
#' @param input_data the input vector
#' @param epsilon privacy budget
#' @param lower_bound lower bound for input values
#' @param upper_bound upper bound for input values
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'split'} or \code{'both'}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.minDP} returns to the client-side a list including: \cr
#' \code{Min.by.Study} (private min) for each study (if \code{type = split} or \code{type = both}). \cr
#' \code{Global.Min} (private min) and across all studies combined (if \code{type = combine} or \code{type = both}).
#' @export

ds.minDP <- function(input_data, epsilon, lower_bound, upper_bound, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("minDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  combined <- min(unlist(res))

  if (type=="combine") return(list(Min.by.Study=res))
  if (type=="split") return(list(Global.Min=combined))
  if (type=="both") return(list(Min.by.Study=res,Global.Min=combined))}
