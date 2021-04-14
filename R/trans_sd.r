#' @title Sd of transformed data
#' @description This is the one-line equivalent of the code show below in examples
#' @param x numeric vector
#' @param y numeric vector
#' @param ref reference vector
#' @importFrom stats ecdf lm qnorm sd
#' @references LIND, Marcus, et al. Variability of INR and its relationship with mortality, stroke, bleeding and hospitalisations in patients with atrial fibrillation. Thrombosis research, 2012, 129.1: 32-35.
#' @examples
#' # The code for trans_sd is equivalent to:
#' # trans_sd <- function(x, y, ref){
#' #    transformation_function <- function(y, ref) qnorm(ecdf(x=ref)(v=y))
#' #    transformed_y <- transformation_function(y=y, ref=ref)
#' #    deviations <- lm(transformed_y ~ x)$residuals
#' #    sd(deviations)
#' # }
#' @export

trans_sd <- function(x, y, ref)
    sd(x = lm(qnorm(p = ecdf(ref)(y)) ~ x)$residuals)


#, @describeIn trans_sd return a function that has calculated the transformation
#,   function already
#, @export

#' @rdname trans_sd
#' @return for \code{trans_sd_fnc} a function of \code{x, y} where the
#'   transformation function has already been calculated (unclear if this is
#'   more efficient)
#' @export

trans_sd_fnc <- function(ref){
    trans <- function(z) qnorm(p = ecdf(ref)(z))
    function(x, y) sd(x = lm(trans(y) ~ x)$residuals)
}
