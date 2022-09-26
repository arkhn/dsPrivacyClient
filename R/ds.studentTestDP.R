source("R/utils.R")

#' @title Differentially private student-test
#'
#' @param x First group to compare with the student-test
#' @param y Second group to compare with the student-test
#' @param epsilon Privacy budget
#' @param x_min Lower bound for x
#' @param x_max Upper bound for x
#' @param y_min Lower bound for y
#' @param y_max Upper bound for y
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'split'} or \code{'both'}.
#'
#' @return \code{ds.studentTestDP} returns a differentially private student-test
#' @export

ds.studentTestDP <- function(x, y, epsilon, x_min, x_max, y_min, y_max, type="combine", datasources=NULL) {
    if (is.null(datasources)) {
        datasources <- DSI::datashield.connections_find()
    }
    if (!type %in% c("both", "split", "combine")) {
        stop("Type must be one of 'both', 'split' or 'combine'")
    }

    split_epsilon <- epsilon/2
    res_x <- getGroupStatistics(datasources, x, split_epsilon, x_min, x_max)
    res_y <- getGroupStatistics(datasources, y, split_epsilon, y_min, y_max)
    
    Nstudies <- length(datasources)
    
    studentTest.combine <- computeStudentTestCombine(Nstudies, res_x, res_y)
    studentTest.split <- computeStudentTestSplit(Nstudies, res_x, res_y)

    if (type == "both") return(list(StudentTest.by.Study=studentTest.split,Global.StudentTest=studentTest.combine,Nstudies=Nstudies))
    if (type == "combine") return(list(Global.StudentTest=studentTest.combine,Nstudies=Nstudies))
    if (type == "split") return(list(StudentTest.by.Study=studentTest.split,Nstudies=Nstudies))
}

computeGlobalStats <- function(Nstudies, input) {
    GlobalSum <- 0
    GlobalSumSquares <- 0
    GlobalNvalid <- 0  
    for (i in 1:Nstudies){
        GlobalSum <- GlobalSum +  input$Sum[[i]]
        GlobalSumSquares <- GlobalSumSquares +  input$SumSquares[[i]]
        GlobalNvalid <- GlobalNvalid +  input$Nvalid[[i]]
    }

    GlobalVar <- max(GlobalSumSquares/(GlobalNvalid-1) - (GlobalSum^2)/(GlobalNvalid*(GlobalNvalid-1)), delta)
    GlobalMean <- GlobalSum/GlobalNvalid
    return (list(GlobalVar=GlobalVar,GlobalMean=GlobalMean,GlobalNvalid=GlobalNvalid))
}

computeStudentTestCombine <- function(Nstudies, input_x, input_y) {
    x_stats <- computeGlobalStats(Nstudies, input_x) 
    y_stats <- computeGlobalStats(Nstudies, input_y)
    StudentTest <- (x_stats$GlobalMean - y_stats$GlobalMean)/sqrt(x_stats$GlobalVar / x_stats$GlobalNvalid + y_stats$GlobalVar / y_stats$GlobalNvalid)
    return (StudentTest)
}

computeLocalStats <- function(Sum, SumSquares, Nvalid){
    Nvalid <- as.numeric(Nvalid)
    Mean <- Sum / Nvalid
    Var <- max(SumSquares/(Nvalid-1) - (Sum^2)/(Nvalid*(Nvalid-1)), delta)
    return (list(Nvalid=Nvalid,Mean=Mean,Var=Var))
}

computeStudentTestSplit <- function(Nstudies, input_x, input_y) {
    StudentTest <- c()
    for (i in 1:Nstudies){
        x_stats <- computeLocalStats(input_x$Sum[[i]], input_x$SumSquares[[i]], input_x$Nvalid[[i]])
        y_stats <- computeLocalStats(input_y$Sum[[i]], input_y$SumSquares[[i]], input_y$Nvalid[[i]])
        StudentTest[i] <- (x_stats$Mean - y_stats$Mean)/sqrt(x_stats$Var / x_stats$Nvalid + y_stats$Var / y_stats$Nvalid) 
    }
    return (StudentTest)
}

getGroupStatistics <- function(datasources, input, epsilon, input_min, input_max) {
    split_epsilon <- epsilon/3
    SumSquares <- callAggregationMethod(datasources, paste0("sumOfSquaresDP(", input, ", ", split_epsilon, ", ", input_min, ", ", input_max, ")"))
    Sum <- callAggregationMethod(datasources, paste0("sumDP(", input, ", ", split_epsilon, ", ", input_min, ", ", input_max, ")"))
    Nvalid <- callAggregationMethod(datasources, paste0("numValidDP(", input, ", ", split_epsilon, ")"))
    res <- list(Sum=Sum, SumSquares=SumSquares, Nvalid=Nvalid)
    return (res)
}