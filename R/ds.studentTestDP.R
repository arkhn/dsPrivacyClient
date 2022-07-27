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
#'
#' @return \code{ds.studentTestDP} returns a differentially private student-test
#' @export


ds.studentTestDP <- function(datasources, x, y, epsilon, x_min, x_max, y_min, y_max, type="combine") {
    if (is.null(datasources)) {
        datasources <- DSI::datashield.connections_find()
    }

    split_epsilon <- epsilon/2

    cally_x <- paste0("calculationsForStudentTestDP(", x, ", ", split_epsilon, ", ", x_min, ", ", x_max, ")")
    cally_y <- paste0("calculationsForStudentTestDP(", y, ", ", split_epsilon, ", ", y_min, ", ", y_max, ")")
    res_x <- DSI::datashield.aggregate(datasources, as.symbol(cally_x))
    res_y <- DSI::datashield.aggregate(datasources, as.symbol(cally_y))
    
    Nstudies <- length(datasources)
    
    studentTest.combine <- computeStudentTestCombine(Nstudies, res_x, res_y)
    studentTest.split <- computeStudentTestSplit(Nstudies, res_x, res_y)

    if (type == "both") {
        return(list(StudentTest.by.Study=studentTest.split,Global.StudentTest=studentTest.combine,Nstudies=Nstudies))
    } else if (type == "combine") {
        return(list(Global.StudentTest=studentTest.combine,Nstudies=Nstudies))
    } else if (type == "split") {
        return(list(StudentTest.by.Study=studentTest.split,Nstudies=Nstudies))
    } else {
        stop("type must be either 'both', 'combine' or 'split'")
    }
}

computeGlobalStats <- function(Nstudies, input) {
    GlobalSum <- 0
    GlobalSumSquares <- 0
    GlobalNvalid <- 0  
    for (i in 1:Nstudies){
        GlobalSum <- GlobalSum +  input[[i]]$Sum
        GlobalSumSquares <- GlobalSumSquares +  input[[i]]$SumSquares
        GlobalNvalid <- GlobalNvalid +  input[[i]]$Nvalid
    }

    GlobalVar <- GlobalSumSquares/(GlobalNvalid-1) - (GlobalSum^2)/(GlobalNvalid*(GlobalNvalid-1))
    GlobalMean <- GlobalSum/GlobalNvalid
    return (list(GlobalVar=GlobalVar,GlobalMean=GlobalMean,GlobalNvalid=GlobalNvalid))
}

computeStudentTestCombine <- function(Nstudies, input_x, input_y) {
    x_stats <- computeGlobalStats(Nstudies, input_x) 
    y_stats <- computeGlobalStats(Nstudies, input_y)
    StudentTest <- (x_stats$GlobalMean - y_stats$GlobalMean)/sqrt(x_stats$GlobalVar / x_stats$GlobalNvalid + y_stats$GlobalVar / y_stats$GlobalNvalid)
    return (StudentTest)
}

computeLocalStats <- function(input_col){
    Nvalid <- as.numeric(input_col$Nvalid)
    Mean <- input_col$Sum / input_col$Nvalid
    Var <- input_col$SumSquares/(Nvalid-1) - (input_col$Sum^2)/(Nvalid*(Nvalid-1))
    return (list(Nvalid=Nvalid,Mean=Mean,Var=Var))
}

computeStudentTestSplit <- function(Nstudies, input_x, input_y) {
    StudentTest <- c()
    for (i in 1:Nstudies){
        x_stats <- computeLocalStats(input_x[[i]])
        y_stats <- computeLocalStats(input_y[[i]])
        StudentTest[i] <- (x_stats$Mean - y_stats$Mean)/sqrt(x_stats$Var / x_stats$Nvalid + y_stats$Var / y_stats$Nvalid) 
    }
    return (StudentTest)
}