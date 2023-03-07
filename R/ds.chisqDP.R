source("R/utils.R")

#' @title Differentially private chi square computation
#'
#' @param x first input vector
#' @param y second input vector
#' @param epsilon privacy budget
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'split'} or \code{'both'}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return \code{ds.chisqDP} returns a differentially private chi square value
#' @export

ds.chisqDP <- function(x, y, epsilon, type="combine", datasources=NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!type %in% c("both", "split", "combine")) {
    stop("Type must be one of 'both', 'split' or 'combine'")
  }

  Nstudies <- length(datasources)

  contingencyTables <- callAggregationMethod(datasources, paste0("tableDP(", x, ", ", y, ", " , epsilon, ")"))

  Chisq.combine <- computeChisqCombine(Nstudies, contingencyTables)
  Chisq.split <- computeChisqSplit(Nstudies, contingencyTables)

  if (type == "both") return(list(Chisq.by.Study=Chisq.split,Global.Chisq=Chisq.combine,Nstudies=Nstudies))
  if (type == "combine") return(list(Global.Chisq=Chisq.combine,Nstudies=Nstudies))
  if (type == "split") return(list(Chisq.by.Study=Chisq.split,Nstudies=Nstudies))
}

computeChisqCombine <- function(Nstudies, contingencyTables) {
    GlobalTable <- 0
    for (i in 1:Nstudies){
      GlobalTable <- GlobalTable + contingencyTables[[i]]
    }

    sr <- rowSums(GlobalTable)
    sc <- colSums(GlobalTable)

    ntotal <- sum(GlobalTable)
    expectedCounts <- (as.matrix(sr) %*% t(as.matrix(sc))) / ntotal
    chisq <- sum(GlobalTable^2/expectedCounts) - ntotal

    return (chisq)
}

computeChisqSplit <- function(Nstudies, contingencyTables) {
    chisq <- c()
    for (i in 1:Nstudies){
      contingencyTable <- contingencyTables[[i]]
      sr <- rowSums(contingencyTable)
      sc <- colSums(contingencyTable)

      ntotal <- sum(contingencyTable)
      expectedCounts <- (as.matrix(sr) %*% t(as.matrix(sc))) / ntotal
      chisq[i] <- sum(contingencyTable^2/expectedCounts) - ntotal
    }
    return (chisq)
}
