#' Adult or larval dispersal
#'
#' @param fish
#' @param cm
#' @param ages
#'
#' @return
#' @export
#'
#' @examples
#'
#' cells=5000
#' cm <- matrix(1/cells,nrow=cells,ncol=cells)
#' fish <- initpop(initial_abun=250*10^6,cells=cells,maxage=50,rate=0.7)
#' fish <- dispersal(fish,cm,ages=5:50)

dispersal <- function(fish,cm,ages=5:50){
    if(length(ages)==1){
        fish[,ages+1] <- roundprob(colSums(fish[,ages+1]*cm))
    } else {
        fish[,ages+1] <- roundprob(apply(fish[,ages+1],2,function(x) colSums(x*cm)))
    }
    return(fish)
}
