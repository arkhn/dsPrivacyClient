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
ds.countDP <- function(datasources, input_data, epsilon, type="combine") {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("countDP(", input_data, ", ", epsilon, ")")
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  combined <- sum(unlist(res))

  if (type=="combine") return(list(Count.by.Study=res))
  if (type=="split") return(list(Global.Count=combined))
  if (type=="both") return(list(Count.by.Study=res,Global.Count=combined))
}
