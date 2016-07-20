#' Title
#'
#' @param p
#' @param oldMPA
#' @param n
#'
#' @return
#' @export
#'
#' @examples
addscenario <- function(p,oldMPA,MPA_coverage=0.1,replicates=10,dist=NA,name="Status_quo",Habitats=FALSE,Breeding=FALSE,cell_size){

    Status_quo <- apply(gCovers(oldMPA,p,byid = TRUE),1,any)|apply(gOverlaps(oldMPA,p,byid = TRUE),1,any)

    if(name=="Status_quo"){

        for(i in replicates){
            p[[paste0("MPA_SQ_",i)]] <- Status_quo
        }

        return(p)

    } else {
        # browser()

        # create neighbours and graphs

        require(igraph)
        require(spdep)

        nb <- poly2nb(p,snap=100)
        gr <- spdf2graph(p,nb)

        # set Breeding protection if applicable
        if(Breeding) Status_quo <- Status_quo|p$Breeding


        # loop through replicates

        for(i in replicates){

            # set status_quo as MPAs
            p[[paste0(name,"_",i)]] <- Status_quo

            # generate new MPA sizes
            MPA_sizes <- generatempasizes(cell_size,p,MPA_coverage,Status_quo)

            # set distances
            if(is.na(dist)){
                dist <- round(sqrt(length(p))/(sqrt(length(MPA_sizes))-1))
            } else {
                dist <- round(dist/cell_size)+1
            }


            for(j in MPA_sizes){
                candidates <- NULL
                tol <- 1
                while(length(candidates)<1){

                    candidates <- unique(unlist(neighborhood(gr,order=dist,node=which(p@data[,names(p)==paste0(name,"_",i)]))))

                    if(Habitats) candidates <- candidates[p$Habitats[candidates]]

                    candidates <- candidates[!candidates %in% unlist(neighborhood(gr,order=dist*tol,node=which(p@data[,names(p)==paste0(name,"_",i)])))]

                    tol <- tol*0.9
                }

                seed <- sample(candidates,1)
                while(length(seed)<j) seed <- unique(c(seed,sample(unlist(nb[seed]))))
                if(length(seed)>j) seed <- seed[1:j]
                # plot(p[seed,],col='blue')
                p[[paste0(name,"_",i)]][seed] <- TRUE

            }

        }
        return(p)

    }

}
