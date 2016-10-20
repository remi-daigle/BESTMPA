#' Title
#'
#' @param fish
#' @param quota
#' @param ages
#' @param distance
#' @param fish_licenses
#' @param mpaSQ
#' @param mpa2020
#' @param writefish
#' @param writefishcatch
#' @param writekg
#'
#' @return
#' @export
#'
#' @examples
harvest <- function(fish,quota,ages=min_age_catch:maxage,distance,fish_licenses,mpaSQ,mpa2020,writefish=TRUE,writefishcatch=TRUE,writekg=TRUE,results_folder){
    if(!y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=mpaSQ)
    if(y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=mpa2020)


    catch <- Reduce("+",fishcatch)
    fish <- fish-catch

    # check for extinctions
    if(any(fish<0)) fish[fish<0] <- 0

    kg <- t(do.call(rbind,lapply(fishcatch,fish2weight,ages)))
    names(kg) <- names(fish_communities)

    # writing to disk

    if(y %in% time & writefish) write.csv(fish,paste0(results_folder,"/fish_",s,"_",y,".csv"))
    if(y %in% time & writefishcatch) write.csv(catch,paste0(results_folder,"/fishcatch_",s,"_",y,".csv"))
    if(y %in% time & writekg) write.csv(kg,paste0(results_folder,"/kg_",s,"_",y,".csv"))

    return(fish)
}
