source("R/utils.R")

#' @title Differentially private count
#'
#' @param target_column target response
#' @param input_columns explanatory variables
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

ds.linearRegressionDP <- function(target_column, input_columns, epsilon, type="split", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  coefficients.split <- callAggregationMethod(datasources, paste0("linearRegressionDP(", target_column, ", ", input_columns, ", ", epsilon, ")"))
  coefficients.combine <- NULL

  if (type=="combine") return(list(Global.Coefficients=coefficients.combine))
  if (type=="split") return(list(Coefficients.by.Study=coefficients.split))
  if (type=="both") return(list(Coefficients.by.Study=coefficients.split,Global.Coefficients=coefficients.combine))
}
