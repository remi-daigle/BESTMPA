#' Title
#'
#' @param domain
#' @param priority
#' @param excluded
#' @param MPA_coverage
#' @param replicates
#' @param dist
#' @param name
#' @param cell_size
#' @param cells
#' @param included
#'
#' @return
#' @export
#'
#' @examples
addscenario <- function(domain,included=NA,priority=NA,excluded=NA,MPA_coverage=0.1,replicates=10,dist=NA,name="Status_quo",cell_size,cells){

    Status_quo <- apply(gCovers(included,domain,byid = TRUE),1,any)|apply(gOverlaps(included,domain,byid = TRUE),1,any)

    if(name=="Status_quo"){

        for(i in replicates){
            domain[[paste0("MPA_SQ_",i)]] <- Status_quo
        }

        return(domain)

    } else {
        # create neighbours and graphs

        require(igraph)
        require(spdep)

        nb <- poly2nb(domain,snap=100)
        gr <- spdf2graph(domain,nb)

        # # set Breeding protection if applicable
        # if(Breeding) Status_quo <- Status_quo|domain$Breeding


        # loop through replicates

        for(i in replicates){
            print(paste("calculating replicate",i))

            # set status_quo as MPAs
            domain[[paste0(name,"_",i)]] <- Status_quo

            # generate new MPA sizes
            MPA_sizes <- generatempasizes(cell_size,cells,domain,MPA_coverage,Status_quo)

            # set distances
            if(is.na(dist)){
                dist <- round(sqrt(length(domain))/(sqrt(length(MPA_sizes))-1))
            } else {
                dist <- round(dist/cell_size)+1
            }


            for(j in MPA_sizes){
                candidates <- NULL
                tol <- 1
                while(length(candidates)<1){

                    candidates <- unique(unlist(neighborhood(gr,order=dist,node=which(domain@data[,names(domain)==paste0(name,"_",i)]))))

                    # remove non-priority and excluded
                    if(!any(is.na(priority))) candidates <- candidates[priority[candidates]]
                    if(!any(is.na(excluded))) candidates <- candidates[!excluded[candidates]]

                    candidates <- candidates[!candidates %in% unlist(neighborhood(gr,order=dist*tol,node=which(domain@data[,names(domain)==paste0(name,"_",i)])))]

                    tol <- tol*0.9
                }

                seed <- sample(candidates,1)
                while(length(seed)<j){
                    seed <- unique(c(seed,sample(unlist(nb[seed]))))
                    # # remove non-priority and excluded
                    if(!any(is.na(priority))) seed <- seed[priority[seed]]
                    if(!any(is.na(excluded))) seed <- seed[!excluded[seed]]
                    }
                if(length(seed)>j) seed <- seed[1:j]
                # plot(domain[seed,],col='blue')
                domain[[paste0(name,"_",i)]][seed] <- TRUE

            }

        }
        return(domain)

    }

}
