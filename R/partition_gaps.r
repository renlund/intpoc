#' @title Partition gaps in time series
#' @description A time series defined by \code{x} (time) and \code{y}
#'   (measurement) is partitioned into shorter pieces whenever the time gap
#'   between consecutive points are greater than (or equal to) \code{gap}.
#' @param x numeric vector
#' @param y numeric vector (equal in length to \code{x})
#' @param gap numeric singelton
#' @param check logical - should we test if parameters are sane?
#' @param mess logical; do you want messages?
#' @importFrom dplyr tibble
#' @return A list of \code{tbl_df} ('tibble') objects
#' @export

partition_gaps <- function(x, y, gap=90, check=TRUE, mess=FALSE){
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
            L[[indx]] <- tibble(
                x = x[(is[indx]+1):is[indx+1]],
                y = y[(is[indx]+1):is[indx+1]]
            )
        }
        L
    } else {
        list(tibble(
            x = x,
            y = y
            ))
    }
}

#' @description \code{partion_tir}: apply \code{tir} to each part of \code{partition_gaps}
#' @describeIn partition_gaps partition tir
#' @param low.tol the minimum number of points in each partition for proceeding
#'    with tir calculation
#' @param ... arguments passed to \code{tir}
#' @note \code{partition_tir} is very BETA!
#' @export

partition_tir <- function(x, y, gap = 90, check=TRUE, mess = FALSE, low.tol = 3, ...){
    g <- partition_gaps(x = x, y = y, gap = gap, check = check, mess = mess)
    null_ret <- tir(1:2,c(0,0), ...)
    null_ret$Time <- NA
    null_ret$TiR  <- NA
    null_ret$percent <- NA
    if(length(x) < low.tol) return(null_ret)
    #gi <- lapply(g, function(x) nrow(x) >= low.tol)
    gi <- lapply(g, function(X) length(unique(X$x)) >= low.tol)
    g <- g[unlist(gi)]
    if(length(g) == 0) return(null_ret)
    fnc <- function(X) tir(x = X$x,  y = X$y, ...)
    tg <- lapply(g, fnc)
    RET <- tg[[1]]
    n <- length(tg)
    if(n>1){
        for(i in 2:n){ # i = 2
            RET$Time <- RET$Time + tg[[i]]$Time
            RET$TiR <- RET$TiR + tg[[i]]$TiR
        }
        RET$percent <- RET$TiR / RET$Time
    }
    RET
}
