#' @title Mean-ish of time series
#' @description Calculate the average of an interpolated curve
#' @param x 'time' variable
#' @param y measurements
#' @param x. point at which to calculate mean
#' @param win window over whitch to calculate mean
#' @param gap tolerance level for distance between entries in \code{x}
#' @param y.NA how should \code{NA} in \code{y} be handled. Currently "remove"
#'   and "interpolate" is available.
#' @param check apply checking function \code{check_num_vec} to \code{x} and
#'   \code{y}?
#' @param mess messages on?
#' @importFrom dplyr data_frame
#' @export

win_mean <- function(x, y, x., win, gap = Inf, y.NA = "interpolate", check=TRUE, mess=FALSE){
    original_sample_size <- length(x)
    y_copy <- na.omit(y)
    if(check) {
        if(typeof(df <- check_num_vec(x, y, pre_mess = "[win_mean]", y.NA = y.NA, mess=mess))!="logical"){
            if(!is.null(df)){
                x <- df$x
                y <- df$y
            }
        } else {
            stop("[win_mean] check_num_vec failed")
        }
    }
    if(!(missing(x.) & missing(win))){
        if(!missing(x.) & missing(win)){
            if(mess) message("[win_mean] 'win' has been set so as to cover all of 'x'")
            win  <- x. - x[1]
        } else if(missing(x.) & !missing(win)) {
            x.  <- x[length(x)]
        }
        # if(missing(x.))  x.  <- x[length(x)]
        # if(missing(win)) win <- diff(range(x))
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
    } else {
        # these values are needed later ?
        x. <- x[length(x)]
        win <- diff(range(x))
    }
    if(max(diff(x))>gap){
        L <- partition_gaps(x = x, y = y, gap = gap, check = FALSE)
        storage <- lapply(L, FUN = win_mean_for_lapply, x.=x., win=win)
        extr <- extract_stuff(storage)
        dplyr::data_frame(
            int = extr["int"],
            sq_int = extr["sq_int"],
            span = extr["span"],
            mean = int / span,
            sd = sqrt(sq_int / span - (mean)^2),
            #             sum = extr["sum"],
            #             sq_sum = extr["sq_sum"],
            #             n_y = extr["n_y"],
            sum = sum(y_copy),
            sq_sum = sum(y_copy^2),
            n_y = length(y_copy),
            raw_mean = sum / n_y,
            raw_sd = sqrt((sq_sum  - n_y * raw_mean^2) / (n_y-1)),
            n_dates = length(x),
            parts = length(storage),
            parts_info = extr["parts"]
        )
    } else {
        dplyr::data_frame(
            int = sum(vecrea(x, y, check=FALSE)),
            sq_int = sum(vecre(x, y, check=FALSE)),
            span = diff(range(x)),
            mean = int / span,
            sd = sqrt(sq_int / diff(range(x)) - (mean)^2),
            sum = sum(y_copy),
            sq_sum = sum(y_copy^2),
            n_y = length(y_copy),
            raw_mean = sum / n_y,
            raw_sd = sqrt((sq_sum  - n_y * raw_mean^2) / (n_y-1)),
            n_dates = length(x),
            parts = 1,
            parts_info = 1 # ?????
        )
    }
}
