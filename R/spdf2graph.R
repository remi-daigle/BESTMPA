#' Title
#'
#' @param spdf
#'
#' @return
#' @export
#'
#' @examples
spdf2graph <- function(spdf,nb){
    require(spdep)
    require(igraph)
    # nb <- poly2nb(spdf,snap)
    df <- NULL
    spts <- SpatialPoints(spdf)
    for(i in seq(length(spdf))){
        df <- rbind(df,data.frame(from=i,to=nb[[i]],distance=c(gDistance(spts[i,],spts[nb[[i]],],byid=TRUE))))
    }
    m <- as.numeric(sapply(1:nrow(df), function(i)(df[i,1:2])))
    gr <- graph.empty(length(spdf)) %>%
        add.edges(m,weight=df$distance)
    return(gr)
}
