#' @title Sd of transformed data
#' @description This is the one-line equivalent of the code show below in examples
#' @param x numeric vector
#' @param y numeric vector
#' @param ref reference vector
#' @references LIND, Marcus, et al. Variability of INR and its relationship with mortality, stroke, bleeding and hospitalisations in patients with atrial fibrillation. Thrombosis research, 2012, 129.1: 32-35.
#' @examples
#' # The code is equivalent to:
#' # trans_sd <- function(x, y, ref){
#' #    transformation_function <- function(y, ref) qnorm(ecdf(x=ref)(v=y))
#' #    transformed_y <- transformation_function(y=y, ref=ref)
#' #    deviations <- lm(transformed_y ~ x)$residuals
#' #    sd(deviations)
#' # }
#' @export

trans_sd <- function(x, y, ref) sd(lm(qnorm(ecdf(ref)(y))~x)$residuals)
