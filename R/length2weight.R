#' Calculate weight from length
#'
#' Calculates weight from lenght using the formula \code{weight <- l_to_w_int * length^l_to_w_power}
#'
#' @param length fish length in cm. can be easily calculated using \code{\link{age2length}}
#' @param l_to_w_int The intercept in the Length-weight relationship
#' @param l_to_w_power The power in the Length-weight relationship
#'
#' @return
#' @export
#'
#' @examples
#' weight <- length2weight(length=50,l_to_w_int=0.000011,l_to_w_power=2.91)
length2weight <- function(length,l_to_w_int=0.000011,l_to_w_power=2.91){
    l_to_w_int*length^l_to_w_power
}
