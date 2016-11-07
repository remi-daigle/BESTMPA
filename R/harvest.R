#' Title
#'
#' @param fish
#' @param quota
#' @param ages
#' @param distance
#' @param fish_licenses
#' @param mpabefore Logical vector, TRUE if closed to fishing before \code{time}
#' @param mpaafter Logical vector, TRUE if closed to fishing during \code{time}
#' @param writefish If TRUE, will write the \code{fish} remaining for each cell/age to the \code{results_folder}
#' @param writefishcatch If TRUE, will write the \code{fish} caught for each cell/age to the \code{results_folder}
#' @param writekg (logical) If TRUE, will write the number of kg of fish caught for each cell/community to the \code{results_folder}
#' @param results_folder Path to the directory where results are stored
#' @param y current year
#' @param time years during which model results are recorded (i.e. not during spin-up time)
#' @inheritParams fishing
#' @inheritParams dispersal
#'
#' @return
#' @export
#'
#' @examples
#' #' # create a fish matrix
#' fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
#' # calculate distances from shore
#' distance <- gDistance(spTransform(fish_communities,proj),BESTMPA_domain,byid = T)
#' # number of licenses per region in fish_communities
#' fish_licenses <- c(866, 4714, 3002, 879, 963)
#' # define MPAs
#' mpa <- sample(c(TRUE,FALSE),length(BESTMPA_domain),replace=TRUE)
#' nompa <- rep(FALSE,length(BESTMPA_domain))
#' # quota
#' quota <- estimatequota(fish,maxage=20,y=2001,tot_time=2001:2071,FMSY=0.28,FMSY_buffer=0.667)
#' # go fishing!
#' fish <- harvest(fish,quota,ages=4:20,distance,fish_licenses,mpabefore=nompa,mpaafter=mpa,writefish=FALSE,writefishcatch=FALSE,writekg=TRUE,results_folder='',y=2001,time=time)
harvest <- function(fish,quota,ages=min_age_catch:maxage,distance,fish_licenses,mpabefore,mpaafter,writefish=TRUE,writefishcatch=TRUE,writekg=TRUE,results_folder,y,time){
    if(!y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=mpabefore)
    if(y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=mpaafter)


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
