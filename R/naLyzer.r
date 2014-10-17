# naLyzer is a helper function and NOT exported

naLyzer <- function(x){
    n <- length(x)
    if(n==0) {
        m <- 0
        l <- 0
        op <- 0
        om <- 0
        u <- 0
    } else {
        m <- sum(is.na(x))
        x1 <- na.omit(x)
        l <- sum(is.character(x1))
        op <- sum(x1==Inf)
        om <- sum(x1==-Inf)
        u <- length(unique(x1))
    }
    data.frame(
        Length = n,
        n.NA = m,
        n.posInf = op,
        n.negInf = om,
        n.character = l,
        unique = u
    )
}
