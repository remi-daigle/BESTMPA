#' Rounds based on probability
#'
#' This will round `x` probabilistically. For example, `roundprob(1.5)` will return `1` in roughly half of the cases and will return `2` in the other cases.
#'
#' @param x a vector of numbers to be rounded
#'
#' @export
#'
#' @examples
#' table(roundprob(rep(1.5,100000)))
roundprob <- function(x){
    floor(x)+as.numeric(x%%1>runif(length(x),min=0,max=1))
}
