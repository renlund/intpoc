# helper functions for package 'intpoc'

dput2 <- function(x){
    cat(gsub(" ", "", paste0(capture.output(dput(x, file="")), collapse=" ")))
}
