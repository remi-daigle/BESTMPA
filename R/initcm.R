#' Initiate the connectivity matrices
#'
#' @despcription generates a connectivity matrix for the model domain based on an exponential dispersal kernel
#'
#' @param domain
#' @param e_fold The e-folding scale for dispersal in m (the distance at which there will be fewer larvae/adults dispersed from a central point by a factor of e)
#' @param cell_size
#' @inheritParams addscenario

#' @return
#' @export
#'
#' @examples
#' initcm(domain=BESTMPA_domain,e_fold=75000,cell_size=20000)
initcm <- function(domain,e_fold,cell_size){
    require(igraph)
    require(spdep)
    require(dplyr)

    nb <- poly2nb(domain,snap=100)
    gr <- spdf2graph(domain,nb)

    distance_matrix <- round(shortest.paths(gr,c(1:4779),c(1:4779))/cell_size)


    conmat <- apply(distance_matrix,1,function(x){
        dist_table <- x %>%
            table() %>%
            as.data.frame(stringsAsFactors=FALSE) %>%
            mutate(dist=as.numeric(.)) %>%  #because '.' causes problems
            mutate(dist_prob = pexp(dist+1,rate=1/(e_fold/cell_size))-pexp(dist,rate=1/(e_fold/cell_size))) %>% # calculate probability that particle goes a particular distance
            mutate(cell_prob = dist_prob/Freq) # partition the above by the number of cells at that distance
        cell_prob <- left_join(data.frame(dist=x),dist_table,by="dist")
        return(cell_prob$cell_prob)
    })
}


