#' Title
#'
#' @param y
#' @param tot_time
#' @param initial_abun
#' @param maxage
#' @param recruits
#' @param M
#' @param fish
#' @param domain
#'
#' @return
#' @export
#'
#' @examples
growth <- function(fish,y,tot_time,initial_abun,domain,maxage,recruits,M){
    #### initiate `fish` or recruit the `recruits` ####
    if(y==min(tot_time)){
        fish <- initpop(initial_abun=250*10^6,cells=length(domain),maxage=maxage,rate=0.7)
    } else {
        fish[,1+1:maxage] <- fish[,1:maxage]
        fish[,1] <- recruits
    }

    # check for extinctions
    if(any(fish<0)) fish[fish<0] <- 0

    #### adult mortality ####
    fish <- mortality(fish,M=sample(M,nrow(fish)),ages=1:maxage)

    # fish outside the habitat die automatically
    fish <- fish*domain$Habitats
}
