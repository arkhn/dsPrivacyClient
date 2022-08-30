source("R/utils.R")

#' @title Differentially private covariance
#'
#' @param x First input to the covariance
#' @param y Second input to the covariance
#' @param epsilon Privacy budget
#' @param x_min Lower bound for x
#' @param x_max Upper bound for x
#' @param y_min Lower bound for y
#' @param y_max Upper bound for y
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.boundedCovarianceDP} returns a differentially private covariance
#' @export

ds.boundedCovarianceDP <- function(x, y, epsilon, x_min, x_max, y_min, y_max, type="split", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!type %in% c("both", "split", "combine")) {
    stop("Type must be one of 'both', 'split' or 'combine'")
  }

  covariance.split <- callAggregationMethod(datasources, paste0("boundedCovarianceDP(", x, ", ", y,  ", ", epsilon, ", ", x_min, ", ", x_max, ", ", y_min, ", ", y_max, ")"))

  if (type == "both") stop("Combine type not implemented") 
  if (type == "combine") stop("NotImplemented") 
  if (type == "split") return(list(Covariance.by.Study=covariance.split,Nstudies=Nstudies))
}
