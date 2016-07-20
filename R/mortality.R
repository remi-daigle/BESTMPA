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
    if(length(ages)==1){
        fish[,ages+1] <- roundprob(fish[,ages+1]*(1-M))
    } else {
        fish[,ages+1] <- roundprob(apply(fish[,ages+1],2,function(x) x*(1-M)))
    }
    return(fish)
}
