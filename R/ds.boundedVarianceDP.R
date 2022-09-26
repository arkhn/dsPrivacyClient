source("R/utils.R")

#' @title Differentially private variance
#'
#' @param input_data the input vector
#' @param epsilon privacy budget
#' @param lower_bound lower bound for input values
#' @param upper_bound upper bound for input values
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'split'} or \code{'both'}.
#'
#' @return \code{ds.boundedVarianceDP} returns a differentially private covariance
#' @export

ds.boundedVarianceDP <- function(input_data, epsilon, lower_bound, upper_bound, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!type %in% c("both", "split", "combine")) {
    stop("Type must be one of 'both', 'split' or 'combine'")
  }

  split_epsilon <- epsilon/3

  SumSquares <- callAggregationMethod(datasources, paste0("sumOfSquaresDP(", input_data, ", ", split_epsilon, ", ", lower_bound, ", ", upper_bound, ")"))
  Sum <- callAggregationMethod(datasources, paste0("sumDP(", input_data, ", ", split_epsilon, ", ", lower_bound, ", ", upper_bound, ")"))
  Nvalid <- callAggregationMethod(datasources, paste0("numValidDP(", input_data, ", ", split_epsilon, ")"))

  Nstudies <- length(datasources)

  variance.combine <- computeVarCombine(Nstudies, Sum, SumSquares, Nvalid)
  variance.split <- computeVarSplit(Nstudies, Sum, SumSquares, Nvalid)

  if (type == "both") return(list(Variance.by.Study=variance.split,Global.Variance=variance.combine,Nstudies=Nstudies))
  if (type == "combine") return(list(Global.Variance=variance.combine,Nstudies=Nstudies))
  if (type == "split") return(list(Variance.by.Study=variance.split,Nstudies=Nstudies))
}

computeVarCombine <- function(Nstudies, Sum, SumSquares, Nvalid) {
    GlobalSum <- 0
    GlobalSumSquares <- 0
    GlobalNvalid <- 0  
    for (i in 1:Nstudies){
        GlobalSum <- GlobalSum +  Sum[[i]]
        GlobalSumSquares <- GlobalSumSquares +  SumSquares[[i]]
        GlobalNvalid <- GlobalNvalid +  Nvalid[[i]]
    }

    GlobalVar <- max(GlobalSumSquares/(GlobalNvalid-1) - (GlobalSum^2)/(GlobalNvalid*(GlobalNvalid-1)), delta)
    return (GlobalVar)
}

computeVarSplit <- function(Nstudies, Sum, SumSquares, Nvalid) {
    LocalVars <- c()
    for (i in 1:Nstudies){
        LocalVars[i] <- max(SumSquares[[i]]/(Nvalid[[i]]-1) - (Sum[[i]]^2)/(Nvalid[[i]]*(Nvalid[[i]]-1)), delta)
    }
    return (LocalVars)
}