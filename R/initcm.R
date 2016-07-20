#' Title
#'
#' @param p
#' @param e_fold
#' @param cell_size
#'
#' @return
#' @export
#'
#' @examples
initcm <- function(p,e_fold,cell_size){
    require(igraph)
    require(spdep)
    require(dplyr)

    nb <- poly2nb(p,snap=100)
    gr <- spdf2graph(p,nb)

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


