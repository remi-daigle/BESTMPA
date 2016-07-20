#' Title
#'
#' @param fish
#' @param M
#' @param ages
#'
#' @return
#' @export
#'
#' @examples
mortality <- function(fish,M,ages=1:50){
    fish[,ages+1] <- roundprob(fish[,ages+1]*(1-M))
    return(fish)
}
