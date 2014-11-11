#' @title Partition gaps in time series
#' @description A time series defined by \code{x} (time) and \code{y}
#'   (measurement) is partitioned into shorter pieces whenever the time gap
#'   between consecutive points are greater than (or equal to) \code{gap}.
#' @param x numeric vector
#' @param y numeric vector (equal in length to \code{x})
#' @param gap numeric singelton
#' @param check logical - should we test if parameters are sane?
#' @importFrom dplyr data_frame
#' @return A list of \code{tbl_df} ('data_frame') objects
#' @export

partition_gaps <- function(x, y, gap=5, check=TRUE){
    if(check) {
        if(typeof(df <- check_num_vec(x, y, pre_mess = "[partition_gaps]", mess=mess))!="logical"){
            if(!is.null(df)){
                x <- df$x
                y <- df$y
            }
        } else {
            stop("[partition_gaps] num_check_vec failed")
        }
    }
    if(length(w <- which(diff(x)>=gap))>0){
        L <- as.list(NULL)
        is <- c(0, w, length(x))
        for(indx in 1:(length(w)+1)){ # indx = 1
            L[[indx]] <- data_frame(
                x = x[(is[indx]+1):is[indx+1]],
                y = y[(is[indx]+1):is[indx+1]]
            )
        }
        L
    } else {
        list(data_frame(
            x = x,
            y = y
            ))
    }
}
