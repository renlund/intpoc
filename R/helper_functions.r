# helper functions for package 'intpoc'

# 'dput2' is only used for tests

dput2 <- function(x){
    cat(gsub(" ", "", paste0(capture.output(dput(x, file="")), collapse=" ")))
}

#' @importFrom dplyr data_frame

check_num_vec <- function(x, y, pre_mess="", y.NA="remove", mess=TRUE){
    changes_made <- FALSE
    if(any(is.na(x))) {
        warning(paste(pre_mess, "no NA:s in x, please"))
        return(invisible(NA))
    }
    n <- length(x)
    if(length(y) != n) {
        warning(paste(pre_mess, "x, y lengths differ"))
        return(invisible(NA))
    }
    if(!y.NA %in% c("r", "remove", "i", "interpolate")){
        warning("[check_num_vec] 'y.NA' should be 'remove' (or 'r') or 'interpolate' (or 'i'). Has been set to 'r'.")
        y.NA <- "remove"
    }
    if(any(diff(sort(x))==0)){
        if(mess) message(paste(pre_mess, "for non-unique x's the corresponding mean of y is used"))
        y <- as.numeric(tapply(X = y, INDEX = x, FUN = mean, na.rm=TRUE, simplify = TRUE))
        x <- sort(unique(x))
        n <- length(x)
        changes_made <- TRUE
    } else {
        o_x <- order(x)
        if(!identical(o_x,1:n)){
            if(mess) message(paste(pre_mess, "x wasn't ordered"))
            y <- y[o_x]
            x <- x[o_x]
            changes_made <- TRUE
        }
    }
    if(n<2) {
        warning(paste(pre_mess), "(unique) x needs to be of length > 1")
        return(invisible(NA))
    }
    y_NA_indx <- is.na(y)
    if(all(y_NA_indx)) {
        warning(paste(pre_mess, "y is too NA-infested"))
        return(invisible(NA))
    }
    if(any(is.na(y))){
        if(y.NA %in% c("i", "interpolate")){
            y <- linRepNA(x, y, F)
            changes_made <- TRUE
            if(mess) message(paste(pre_mess, "'y' had NA:s which were interpolated"))
        }
        if(y.NA %in% c("r", "remove")){
            x <- x[!is.na(y)]
            y <- y[!is.na(y)]
            changes_made  <- TRUE
        }
    }
    if(changes_made) data_frame(x=x, y=y) else invisible(NULL)
}

#' @importFrom dplyr data_frame

windowize <- function(x, y, x., win, check=TRUE, pre_mess = "", mess=FALSE){
    changes_made <- FALSE
    if(check) {
        if(typeof(df <- check_num_vec(x, y, pre_mess = "[windowize]", mess=mess))!="logical"){
            if(!is.null(df)){
                x <- df$x
                y <- df$y
                changes_made <- TRUE
            }
        } else {
            if(mess) warning("[windowize] check_num_vec failed")
            return(invisible(NA))
        }
    }
    n <- length(x)
    if(missing(x.)) {
        x. <- x[n]
        if(mess) message(paste(pre_mess, "latest timepoint used as 'x.'"))
    }
    if(win_missing <- missing(win)){
        win <- diff(range(x))
        if(mess) message(paste(pre_mess, "span x used as 'win'"))
    }
    if(x.-win>=x[n] | x.<=x[1]) {
        if(mess) warning(paste(pre_mess, "'x.' out of range (of 'x')"))
        return(invisible(NA))
    } else if(x.<=x[n]) {
        if(x. %in% x){
            if(x.!=x[n]){
                y <- y[x<=x.]
                x <- x[x<=x.]
                changes_made <- TRUE
            }
        } else {
            i <- sum(x<x.)
            L <- line(x[i:(i+1)], y[i:(i+1)], check = FALSE)
            y_new <- unname(L['intersect'] + L['slope']*x.)
            x <- c(x[1:i], x.)
            y <- c(y[1:i], y_new)
            changes_made <- TRUE
        }
    }
    if(!win_missing){
        low <- x.-win
        if(low>x[1]){
            if(low %in% x){
                y <- y[x>=low]
                x <- x[x>=low]
            } else {
                i <- sum(x<low)
                L <- line(x[i:(i+1)], y[i:(i+1)], check = FALSE)
                y_new <- unname(L['intersect'] + L['slope']*low)
                x <- c(low, x[(i+1):n])
                y <- c(y_new, y[(i+1):n])
            }
            changes_made <- TRUE
        }
    }
    if(changes_made) data_frame(x=x, y=y) else invisible(NULL)
}

check_num_pair <- function(x, y, pre_mess=""){
    if(length(x)!=2 | length(y)!=2) stop(paste(pre_mess, "'x', 'y' should be length 2"))
    if(!is.numeric(x) | !is.numeric(y)) stop(paste(pre_mess, "'x', 'y' should be numeric"))
    if(x[1]>=x[2]) stop(paste(pre_mess, "values in 'x' should be ordered and distinct"))
    if(any(is.na(c(x,y)))) stop(paste(pre_mess, "no NA:s allowed"))
    invisible(NULL)
}

line <- function(x, y, check=TRUE){
    if(check) check_num_pair(x,y, "[line]")
    a <- if(x[1]==0)
        y[1]
    else if(x[2]==0)
        y[2]
    else
        (x[1]*y[2] - x[2]*y[1]) / (x[1] - x[2])
    b <- (y[1]-y[2]) / (x[1]-x[2])
    c("intersect"=a, "slope"=b)
}

h_cut <- function(x, y, h, between=TRUE, check=TRUE){
    check_num_pair(x, y, "[h_cut]")
    isit <- min(y) <= h & max(y) >= h
    if(between & !isit) return(NULL)
    L <- line(x, y, check=FALSE)
    if(L['slope']==0) return(NULL)
    unname((h-L['intersect']) / L['slope'])
}

pairea <-function(x, y, check=TRUE){
    if(check) check_num_pair(x, y, "[pairea]")
    0.5 * diff(x) * sum(y)
}

vecrea <- function(x, y, check=TRUE){
    if(check) {
        if(!is.null(df <- check_num_vec(x, y, pre_mess = "[vecrea]"))){
            if(!"logical" %in% class(df)){
                x <- df$x
                y <- df$y
            }
        }
    }
    m <- length(x)-1
    s <- rep(NA_real_, m)
    for(k in 1:m){
        s[k] <- pairea(x[k:(k+1)], y[k:(k+1)], check=FALSE)
    }
    s
}

paire <- function(x, y, check=TRUE){
    if(check) check_num_pair(x, y, "[pairsq]")
    L <- line(x, y, check=FALSE)
    a <- unname(L['intersect'])
    b <- unname(L['slope'])
    a^2*diff(x) + a*b*diff(x^2) + b^2*diff(x^3) / 3
}

vecre <- function(x, y, check=TRUE){
    if(check) {
        if(!is.null(df <- check_num_vec(x, y, pre_mess = "[vecre]"))){
            if(!"logical" %in% class(df)){
                x <- df$x
                y <- df$y
            }
        }
    }
    m <- length(x)-1
    s <- rep(NA_real_, m)
    for(k in 1:m){
        s[k] <- paire(x[k:(k+1)], y[k:(k+1)], check=FALSE)
    }
    s
}

win_mean_for_lapply <- function(df, x., win, check=FALSE, gap=Inf, mess=FALSE){
    if(nrow(df)>1){
        win_mean(
            x = df$x,
            y = df$y,
            x. = x.,
            win = win,
            check = check,
            mess = mess
        )
    } else {
        NULL
    }
}

extract_stuff <- function(storage){
    get <- c("int", "sq_int", "span", "sum", "sq_sum", "n_y", "parts", "parts_info")
    n <- length(storage)
    r <- matrix(NA_real_, nrow=n, ncol=length(get))
    colnames(r) <- get
    for(indx in 1:n){ # indx = 1
        if(!is.null(storage[[indx]])){
            r[indx,] <- as.numeric(storage[[indx]][get])
        }
    }
    colSums(r, na.rm = TRUE)
}
