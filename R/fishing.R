#' Go fish!
#'
#' Creates a list of \code{fish} that were caught for each community
#'
#' @param fish
#' @param quota fishable quota, can be estimated using \code{link{estimatequota}}
#' @param ages age of catchable fish
#' @param fish_licenses number of licenses per region in \code{fish_communities}
#' @param mpa Logical vector, TRUE if closed to fishing
#' @param distance matrix of distance from each cell to each \code{fish_communities}
#' @inheritParams dispersal
#'
#' @return
#' @export
#'
#' @examples
#' # create a fish matrix
#' fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
#' # calculate distances from shore
#' distance <- gDistance(spTransform(fish_communities,proj),BESTMPA_domain,byid = T)
#' # number of licenses per region in fish_communities
#' fish_licenses <- c(866, 4714, 3002, 879, 963)
#' # define MPAs
#' mpa <- sample(c(TRUE,FALSE),length(BESTMPA_domain),replace=TRUE)
#' # quota
#' quota <- estimatequota(fish,maxage=20,y=2001,tot_time=2001:2071,FMSY=0.28,FMSY_buffer=0.667)
#' # go fishing!
#' fishcatch <- fishing(fish,quota,ages=4:50,distance,fish_licenses,mpa)
fishing <- function(fish,quota,ages=4:50,distance,fish_licenses,mpa){
    # browser()
    haul <- quota/sum(fish_licenses)
    if(haul<10) haul=10
    fish <- fish*(1-mpa)
    kg <- apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
    relative_kg <- kg^2/max(kg^2)
    relative_dist <- 1-t(t(distance)/apply(distance,2,max))
    effort <- relative_kg*relative_dist
    meanweight <- mean(sum(kg)/sum(fish[,ages+1]))
    fishcatch <- replicate(5,fish*0,simplify=FALSE)
    catch <- 0
    # if(sum(kg)<10000) return(fishcatch)
    while(catch<quota){
        cttemp <- Reduce("+",fishcatch)
        for(c in sample(seq(length(fish_licenses)))){
            deployments <- round(kg/haul/2)
            target <- rep(order(effort[,c],decreasing=TRUE),deployments[order(effort[,c],decreasing=TRUE)])
            if(length(target)>fish_licenses[c]){
                target <- target[1:fish_licenses[c]]
            } else {
                target <- c(target,sample(1:nrow(fish),fish_licenses[c]-length(target)))
            }
            catchtable <- as.numeric(table(factor(target,levels = (1:nrow(fish))))*haul/meanweight)
            fishcatch[[c]][,ages+1] <- fishcatch[[c]][,ages+1]+fish[,ages+1]/rowSums(fish[,ages+1])*catchtable
            fishcatch[[c]][is.nan(fishcatch[[c]])] <- 0
            catchtotal <- Reduce("+",fishcatch)
            fishcatch[[c]][catchtotal>fish] <- (fishcatch[[c]]-catchtotal+fish)[catchtotal>fish]
            effort[unique(target),] <- Inf
        }
        catchtotal <- Reduce("+",fishcatch)

        if(sum(catchtotal-cttemp)<1) break

        # print(sum(cttemp-catchtotal))
        catch <- sum(apply(catchtotal[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x)))
        kg <- apply((fish-catchtotal)[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
        relative_kg <- kg^3/max(kg^3)
        relative_dist <- t(t(distance)/apply(distance,2,max))
        effort <- (1-relative_kg)*relative_dist
        # print(catch)
        # if(catch>quota) break
    }
    return(lapply(fishcatch,roundprob))
}
