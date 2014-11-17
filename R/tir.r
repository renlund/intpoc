#' @title Time In Range for 'time-series' like series... sort of.
#' @description this function does the same as TiR but now we can partition the
#' x- AND y-axis into parts; calculating the TiR for each part of the
#' interpolated curve
#' @param x numerical; x-coordinates of points of interest
#' @param y numerical; y-coordinates of points of interest
#' @param y.int numerical; the INR intervals of interest
#' @param x.int numerical, the time intervals of interest
#' @param group logical; if \code{TRUE} 'Group.mean' (no!) will be called to
#'       handle the situation of non-unique x-values
#'       NOTE; if TRUE this may cause some missing y-coordinates
#'       to "disappear". E.g. if a fixed x-value occurs multiple times
#'       (in the x-vector), then \code{group = TRUE} will replace these values with
#'       the single unique x-value and the corresponding mean (non-\code{NA}) y-value
#' @param y.NA character; "allow"="a", "warn"="w" or "stop"="s". This tells
#'       the function how to deal with missing y-coordinates. If
#'       "stop", then no missing values are allowed, so the function stops.
#'       If "allow" or "warn" then \code{rep_na(x,y)} is used to replace
#'       the missing values with the linearly interpolated value or
#'       the "last observation carried forward" if the missing value is
#'       is at the end of the vector (and analagously if the missing value
#'       is at the beginning. If "warn" then the function will also warn
#'       that this has been done. ("allow" if missing.)
#' @param incl.low logical; should lower bound be inclusive (only when \code{length(y.int)==2})
#' @param par.test logical; should we test for sane parameter values?
#' @param mess logical; do you want messages? (default \code{FALSE})
#' @return A \code{data.frame} with
#' \code{R.l} = lower part of the interval for which TiR is calculated
#' \code{R.h} = upper part of the interval for which TiR is calculated
#' \code{T.l} = lower part of the time interval for which TiR is calculated
#' \code{T.h} = upper part of the time interval for which TiR is calculated
#' \code{Time} = the amount of "time" available in (T.l,T.h)
#' \code{TiR} = Time in Range with respect to (R.l,R.h] and (T.l,T.h)
#' \code{percent} = TiR/Time
#' @export

tir  <-  function(x, # SKA DEN INTE HETA TIR?????
                  y,
                  y.int,
                  x.int,
                  group,
                  y.NA,
                  incl.low,
                  par.test = TRUE,
                  mess = FALSE){
    # FIRST PART makes sure arguments are ok  -------------------------->
    nx <- length(x)
    if( par.test) {
        if(missing(y.NA)) y.NA <- "a"
        YNA <- c("allow","a","warn","w","stop","s")
        if(!is.element(y.NA,YNA)) stop("[tir]: y.NA must be 'allow', 'warn' or 'stop'")
        if( missing(group) ) group <- TRUE
        if( !is.logical(group) ) stop("[tir]: 'group' must be logical")
        if( length(y)!=nx ) stop("[tir]: x and y of unequal length")
        if( nx==0 ) stop("[tir]: x,y has length 0")
        if( missing( y.int ) ) {
            if(mess) message("[tir]: 'y.int' missing and set to [-Inf, Inf]")
            y.int <- c(-Inf,Inf)
        }
        if(length(y.int)<2 | any(is.na(y.int))) {
            stop("[tir]: 'y.int' must be of length >1 and not contain missing values")
        }
        if( missing(incl.low) ) {
            incl.low <- FALSE
            if( length(y.int) == 2 ) {
                if(mess)
                    message("[tir]: 'y.int' has length 2 and incl.low is set to FALSE")
            }
        }
        if( !is.logical(incl.low) ) stop("[tir]: 'incl.low' must be logical")
        if( incl.low & length(y.int) > 2 ){
            warning("[TTR:] 'incl.low' set to FALSE since 'y.int' has length >2")
        }
        if( missing(x.int) ) {
            if(mess) message("[tir]: 'x.int' missing, default is range of 'x'")
            x.int <- c( min(x), max(x) )
        }
        if( length(x.int)<2 | any(is.na(x.int))) {
            stop("[tir]: 'x.int' must be of length >1 and not contain missing values")
        }
        if( sum(is.na(x))!=0 ) stop("[tir]: x cannot have missing values")
        if( min(diff(x.int)) <= 0 ) stop("[tir]: 'x.int' is degenerate")
        if( min(diff(x)) < 0 ) stop("[tir]: 'x' is degenerate")

        if( min(diff(x)) == 0 ) {
            if(!group) {
                stop("[tir]: 'x' degenerate (diff.=0 and 'group'=F)")
            } else {
                y <- as.numeric(tapply(X=y,INDEX=x,FUN=mean, na.rm=TRUE))
                x <- unique(x)
                nx <- length(x)
            }
        }
        if(any(is.na(y))) {
            if(y.NA %in% YNA[3:4]) warning("[tir]: y has missing values")
            if(y.NA %in% YNA[5:6]) stop("[tir]: y has missing values")
        }
        if(is.element(y.NA,YNA[1:4])) y <- rep_na(x,y)
    }
    # END OF FIRST PART   <----------------------------------------------
    N <- length(x.int)
    M <- length(y.int)
    R.l <- rep( y.int[1:(M-1)], each=N-1)
    R.h <- rep( y.int[2:M]    , each=N-1)
    Time.l <- c()
    Time.h <- c()
    Time <- c()
    TiR <- c()

    for( j in 1:(M-1) ) {
        S <- rep(NA,N)
        K <- rep(NA,N)
        for( k in 1:(N-1) ) {
            temp_x.int  <- c( x.int[k], x.int[k+1] )
            temp_y.int <- c( y.int[j], y.int[j+1] )
            tmp_T <- rep(NA,nx-1)
            for( i in 1:(nx-1) ){
                tmp_T[i] <- tir.for.pair(x=c(x[i],x[i+1]),
                                         y=c(y[i],y[i+1]),
                                         y.int=temp_y.int,
                                         x.int=temp_x.int,
                                         y.int.order='w',
                                         x.int.order='w',
                                         incl.low= incl.low,
                                         par.test = par.test)
            }
            S[k+1] <- sum(tmp_T)
            if( x[1]>=temp_x.int[2] | x[nx]<= temp_x.int[1] )  {
                K[k+1] <- 0
            } else {
                if( x[1]< temp_x.int[1] ) {
                    x1 <- temp_x.int[1]
                } else {
                    x1 <- x[1]
                }
                if( x[nx]> temp_x.int[2] ) {
                    x2 <- temp_x.int[2]
                } else {
                    x2 <- x[nx]
                }
                K[k+1] <- x2-x1
            }
        }
        Time.l <- c(Time.l, x.int[1:(N-1)])
        Time.h <- c(Time.h, x.int[2:N])
        Time <- c(Time, K[2:N])
        TiR <- c(TiR, S[2:N])
    }

    L <-  (N-1)*(M-1)
    percent <- rep( NA, L )
    percent[ Time!=0 ] <- TiR[ Time!=0 ] / Time[ Time!=0 ]
    D <- data.frame(R.l,R.h,Time.l,Time.h,Time,TiR,percent)
    return(D)
}
