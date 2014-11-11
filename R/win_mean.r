#' @title Mean-ish of time series
#' @description Calculate the average of an interpolated curve
#' @param x 'time' variable
#' @param y measurements
#' @param x. point at which to calculate mean
#' @param win window over whitch to calculate mean
#' @param check apply checking function \code{check_num_vec} to \code{x} and \code{y}?
#' @param mess messages on?
#' @importFrom dplyr data_frame
#' @export

win_mean <- function(x, y, x., win, check=TRUE, mess=FALSE){
    y_copy <- na.omit(y)
    if(check) {
        if(typeof(df <- check_num_vec(x, y, pre_mess = "[win_mean]", mess=mess))!="logical"){
            if(!is.null(df)){
                x <- df$x
                y <- df$y
            }
        } else {
            stop("[win_mean] num_check_vec failed")
        }
    }
    if(!(missing(x.) & missing(win))){
        if(missing(x.))  x.  <- x[length(x)]
        if(missing(win)) win <- diff(range(x))
        if(typeof(df <- windowize(x, y, x., win,
                                  check=FALSE,
                                  pre_mess="[win_mean]",
                                  mess=FALSE)) != "logical"){
            if(!is.null(df)){
                x <- df$x
                y <- df$y
            }
        } else {
            stop("[win_mean] windowize fail" )
        }
    }
    data_frame(
        int = sum(vecrea(x, y, check=FALSE)),
        sq_int = sum(vecre(x, y, check=FALSE)),
        mean = int / diff(range(x)),
        sd = sq_int / diff(range(x)) - (mean)^2,
        raw_mean = mean(y_copy),
        raw_sd = sd(y_copy)
    )
}
