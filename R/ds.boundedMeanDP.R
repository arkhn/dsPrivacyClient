#' Title
#'
#' @param object
#' @param option
#' @param datasources
#'
#' @return
#' @export
#'
#' @examples


ds.boundedMeanDP <- function(datasources, input_data, epsilon, lower_bound, upper_bound, type="combine") {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boundedMeanDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  ss.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  Nstudies <- length(datasources)
  ss.mat <- matrix(as.numeric(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,1:2]),nrow=Nstudies)

  ss.mat.combined <- (t(matrix(ss.mat[,2]))%*%ss.mat[,1])/sum(ss.mat[,2])

  if (type=="combine") return(list(Mean.by.Study=ss.mat[,1],Nstudies=Nstudies))
  if (type=="split") return(list(Global.Mean=ss.mat.combined,Nstudies=Nstudies))
  if (type=="both") return(list(Mean.by.Study=ss.mat[,1],Global.Mean=ss.mat.combined,Nstudies=Nstudies))
}
