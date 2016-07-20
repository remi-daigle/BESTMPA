#' Initialize population matrix
#'
#' @param initial_abun
#' @param cells
#' @param maxage
#' @param rate
#'
#' @return
#' @export
#'
#' @examples
#' fish <- initpop(initial_abun=250*10^6,cells=5000,maxage=50,rate=0.9)
initpop <- function(initial_abun=250*10^6,cells=5000,maxage=50,rate=0.9){
    prob <- pexp(0:maxage+1,rate)-pexp(0:maxage,rate)
    pop <- matrix(initial_abun/cells*prob,nrow=cells,ncol=maxage+1,byrow = TRUE)
    pop <- roundprob(pop)
    colnames(pop) <- paste("age_",0:maxage)
    return(pop)
}
