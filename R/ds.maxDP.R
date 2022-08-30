source("R/utils.R")

#' @title Differentially private max
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
#' @return \code{ds.maxDP} returns to the client-side a list including: \cr
#' \code{Max.by.Study} (private max) for each study (if \code{type = split} or \code{type = both}). \cr
#' \code{Global.Max} (private max) and across all studies combined (if \code{type = combine} or \code{type = both}).
#' @export

ds.maxDP <- function(input_data, epsilon, lower_bound, upper_bound, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!type %in% c("both", "split", "combine")) {
    stop("Type must be one of 'both', 'split' or 'combine'")
  }

  max.split <- callAggregationMethod(datasources, paste0("maxDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")"))
  max.combine <- max(unlist(max.split))

  if (type=="combine") return(list(Global.Max=max.combine))
  if (type=="split") return(list(Max.by.Study=max.split))
  if (type=="both") return(list(Max.by.Study=max.split,Global.Max=max.combine))
}
