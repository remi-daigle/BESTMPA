#' Title
#'
#' @param length
#' @param l_to_w_int
#' @param l_to_w_power
#'
#' @return
#' @export
#'
#' @examples
length2weight <- function(length,l_to_w_int=0.000011,l_to_w_power=2.91){
    l_to_w_int*length^l_to_w_power
}
