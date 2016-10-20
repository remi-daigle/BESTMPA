#' Title
#'
#' @param y
#' @param tot_time
#' @param initial_abun
#' @param p
#' @param maxage
#' @param recruits
#' @param M
#'
#' @return
#' @export
#'
#' @examples
growth <- function(fish,y,tot_time,initial_abun,p,maxage,recruits,M){
    #### initiate `fish` or recruit the `recruits` ####
    if(y==min(tot_time)){
        fish <- initpop(initial_abun=250*10^6,cells=length(p),maxage=maxage,rate=0.7)
    } else {
        fish[,1+1:maxage] <- fish[,1:maxage]
        fish[,1] <- recruits
    }

    # check for extinctions
    if(any(fish<0)) fish[fish<0] <- 0

    #### adult mortality ####
    fish <- mortality(fish,M=sample(M,nrow(fish)),ages=1:maxage)

    # fish outside the habitat die automatically
    fish <- fish*p$Habitats
}
