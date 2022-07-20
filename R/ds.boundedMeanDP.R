#' @title Differentially private mean
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
#' @return \code{ds.boundedMeanDP} returns to the client-side a list including: \cr
#' \code{Mean.by.Study} (private mean) and
#' \code{Ntotal} (sum of missing and valid observations) 
#' separately for each study (if \code{type = split} or \code{type = both}). \cr
#' \code{Global.Mean} (private mean) and \code{Ntotal} 
#' across all studies combined (if \code{type = combine} or \code{type = both}). \cr
#' \code{Nstudies}: number of studies being analysed.
#' @export

ds.boundedMeanDP <- function(input_data, epsilon, lower_bound, upper_bound, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boundedMeanDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  res.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  Nstudies <- length(datasources)
  res.mat <- matrix(as.numeric(matrix(unlist(res.obj),nrow=Nstudies,byrow=TRUE)[,1:2]),nrow=Nstudies)

  res.mat.combined <- (t(matrix(res.mat[,2]))%*%res.mat[,1])/sum(res.mat[,2])

  if (type=="combine") return(list(Mean.by.Study=res.mat[,1],Nstudies=Nstudies))
  if (type=="split") return(list(Global.Mean=res.mat.combined,Nstudies=Nstudies))
  if (type=="both") return(list(Mean.by.Study=res.mat[,1],Global.Mean=res.mat.combined,Nstudies=Nstudies))
}
