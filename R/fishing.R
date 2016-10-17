#' Title
#'
#' @param fish
#' @param quota
#' @param ages
#' @param fish_licenses
#' @param mpa
#' @param distance
#'
#' @return
#' @export
#'
#' @examples
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
