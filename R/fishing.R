#' Title
#'
#' @param fish
#' @param quota
#' @param ages
#' @param distance
#' @param haul
#'
#' @return
#' @export
#'
#' @examples
fishing <- function(fish,quota,ages=4:50,distance,fish_licenses,mpa){
    haul <- quota/sum(fish_licenses)
    fish <- fish*(1-mpa)
    kg <- apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
    distance[kg<haul,] <- Inf # these cells are effectively fished out
    fishcatch <- fish*0
    catch <- 0
    while(catch<quota){
        dist <- apply(distance,2,min)
        for(c in sample(seq(length(fish_licenses)))){
            target <- sample(which(distance[,c]==dist[c]),fish_licenses[c],replace = TRUE)
            catchtable <- as.numeric(table(factor(target,levels = (1:nrow(fish))))*haul)

            fishcatch[,ages+1] <- fishcatch[,ages+1]+fish[,ages+1]/rowSums(fish[,ages+1])*catchtable
            fishcatch[is.nan(fishcatch)] <- 0
            fishcatch[fishcatch>fish] <- fish[fishcatch>fish]
            catch <- sum(apply(fishcatch[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x)))
            distance[distance[,c]==dist[c],c] <- Inf
            # print(catch)
            if(catch>quota) break
        }

    }
    return(fishcatch)
}
