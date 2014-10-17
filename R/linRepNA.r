#' @title Replace \code{NA}:s with interpolated values
#' @description given a sequence of points with x-coordinates 'x' and
#'   y-coordinates 'y' (such that there is at least one point with non-missing
#'   y-coord.) this function linearly interpolates between points and replaces
#'   missing y-coordinates with the interpolated value. If the missing values
#'   are at the end, they are replaced with the nearest non-missing value.
#'   Parameters ----------
#' @param x numerical; x-coordinates of the points of interest
#' @param y numerical; y-coordinates of the points of interest
#' @param display logical; if TRUE then x, y and the replaced y is shown, for
#'   evaluation purpose. (FALSE if missing.)
#' @return numeric vector of interpolated y-values  UNLESS display = TRUE in
#'   which case one gets a dataframe of input AND output.
#' @export


linRepNA <- function(x,
                      y,
                      par.test =TRUE,
                      display = FALSE){
    # FIRST PART makes sure arguments are ok ------------->
    n <- length(y)
    if(par.test) {
        if(!is.logical(display)) {
            stop("[Replace.NA]: 'display' must be logical")
        }
        if(length(x)!=n) {
            stop("[Replace.NA]: 'x' and 'y' must be of the same length")
        }
        if(n==0) {
            stop("[Replace.NA]: 'y' cannot have length = 0")
        }
        if(sum(as.numeric(is.na(x)))!=0) {
            stop("[Replace.NA]: 'x' cannot contain missing values")
        }
        if( n > 1 ) {
            if(min(diff(x))<=0) {
                stop("[Replace.NA]: 'x' must be strictly increasing")
            }
        } else {
            if( is.na(y) ) {
                stop("[Replace.NA]: the singleton 'y' is NA")
            } else {
                return( y )
            }
        }
        if(sum(is.na(y))==0) {
            if(display) {
                warning("[Replace.NA]: No missing values, nothing displayed.")
            }
            return(y)
        }
    }
    # END OF FIRST PART <----------------------------------
    temp <- as.numeric(!is.na(y))*1:n
    nr.coord <- temp[temp!=0]
    if(length(nr.coord)==0) {
        stop("[Replace.NA]: 'y' must have at least one non-missing value")
    }

    minst  <- min(nr.coord)
    storst <- max(nr.coord)
    copy_y <- y
    if(minst!=1) {
        for(i in 1:(minst-1)) {
            copy_y[i]<-y[minst]
        }
    }
    if(storst!=n) {
        for(i in n:(storst+1)) {
            copy_y[i]<-y[storst]
        }
    }
    temp <- as.numeric(is.na(copy_y))*1:n
    na.coord <- temp[temp!=0]
    m <- length(na.coord)
    if(m==0) {
        return(copy_y)
    }
    for(i in 1:m) {
        temp <- na.coord[i]
        c1 <- max(nr.coord[nr.coord<temp])
        c2 <- min(nr.coord[nr.coord>temp])
        if((x[c2]-x[c1])==0) {
            cat(m)
            stop("[Replace.NA]: Cannot divide by zero")
        }
        copy_y[temp] <-
            ((y[c2]-y[c1])*x[temp]+y[c1]*x[c2]-y[c2]*x[c1])/(x[c2]-x[c1])
    }
    if(display)  {
        DF <- data.frame(x,y,copy_y)
        names(DF) <- c("Given x:","and y:","Replace.NA(x,y):")
        return(DF)
    } else {
        return(copy_y)
    }
}
