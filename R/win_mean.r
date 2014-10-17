#' @title Mean-ish of time series
#' @description Calculate the average of an interpolated curve
#' @param x 'time' variable
#' @param y measurements
#' @param x. point at which to calculate mean
#' @param win window over whitch to calculate mean
#' @export

win_mean <- function(x, y, x., win){
    n <- length(x)
    if(length(y) != x) stop("[win_mean] x, y lengths differ")
    if(any(is.na(x))) stop("[win_mean] no NA:s in x, please")
    o_x <- order(x)
    if(!identical(o_x,1:n)){
        message("[win_mean] x wasn't ordered")
        y <- y[o_x]
        x <- x[o_x]
    }
    if(any(is.na(y))){
        y <- linRepNA(x, y, F, F)
        message("[win_mean] 'y' had NA:s which were interpolated")
    }
    if(missing(x.)) {
        x. <- x[n]
        message("[win_mean] latest timepoint used as 'x.'")
    }
    "woooha!"
}

check_num_pair <- function(x, y, pre_mess=""){
    if(length(x)!=2 | length(y)!=2) stop(paste(pre_mess, "'x', 'y' should be length 2"))
    if(!is.numeric(x) | !is.numeric(y)) stop(paste(pre_mess, "'x', 'y' should be numeric"))
    if(x[1]>=x[2]) stop(paste(pre_mess, "values in 'x' should be ordered and distinct"))
    if(any(is.na(c(x,y)))) stop(paste(pre_mess, "no NA:s allowed"))
    invisible(NULL)
}

line <- function(x,y){
    check_num_pair(x,y, "[line]")
    a <- if(x[1]==0)
        y[1]
    else if(x[2]==0)
        y[2]
    else
        (x[1]*y[2] - x[2]*y[1]) / (x[1] - x[2])
    b <- (y[1]-y[2]) / (x[1]-x[2])
    c("intersect"=a, "slope"=b)
}

h_cut <- function(x, y, h, between=TRUE){
    check_num_pair(x, y, "[h_cut]")
    isit <- min(y) <= h & max(y) >= h
    if(between & !isit) return(NULL)
    L <- line(x, y)
    if(L['slope']==0) return(NULL)
    as.numeric((h-L['intersect']) / L['slope'])
}

pairea <- function(x, y){
    check_num_pair(x, y, "[pairea]")
    base <- diff(x)
    if(diff(y)==0){
        return( base * y[1] )
    } else if(min(y)>=0 | max(y)<=0){
        return( base * sum(y) / 2 )
    } else {
        x. <- h_cut(x, y, h=0, between=TRUE)
        s1 <- pairea(x=c(x[1], x.), y=c(y[1], 0))
        s2 <- pairea(x=c(x., x[2]), y=c(0, y[2]))
        return( s1 + s2)
    }
}
