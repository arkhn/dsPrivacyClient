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
ds.minDP <- function(datasources, input_data, epsilon, lower_bound, upper_bound, type="combine") {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("minDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  combined <- min(unlist(res))

  if (type=="combine") return(list(Min.by.Study=res))
  if (type=="split") return(list(Global.Min=combined))
  if (type=="both") return(list(Min.by.Study=res,Global.Min=combined))}
