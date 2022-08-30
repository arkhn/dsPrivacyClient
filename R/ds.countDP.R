source("R/utils.R")

#' @title Differentially private count
#'
#' @param input_data the input vector
#' @param epsilon privacy budget
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'split'} or \code{'both'}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.countDP} returns to the client-side a list including: \cr
#' \code{Count.by.Study} (private count) for each study (if \code{type = split} or \code{type = both}). \cr
#' \code{Global.Count} (private count) and across all studies combined (if \code{type = combine} or \code{type = both}).
#' @export

ds.countDP <- function(input_data, epsilon, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  count.split <- callAggregationMethod(datasources, paste0("countDP(", input_data, ", ", epsilon, ")"))
  count.combine <- sum(unlist(count.split))

  if (type=="combine") return(list(Global.Count=count.combine))
  if (type=="split") return(list(Count.by.Study=count.split))
  if (type=="both") return(list(Count.by.Study=count.split,Global.Count=count.combine))
}
