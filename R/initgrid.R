#' Title
#'
#' @param EEZ
#' @param cell_size
#' @param proj
#' @param areas
#'
#' @return
#' @export
#'
#' @examples
initgrid <- function(EEZ,cell_size,proj,areas=NA){
    # browser()
    if(is.null(names(areas))) warning("areas are not named (e.g. areas=c(Breeding=Breeding))")

    EEZ <- spTransform(EEZ,CRS(proj))
    if(!any(is.na(areas))){
        for(i in seq_along(areas)){
            areas[[i]] <- spTransform(areas[[i]],CRS(proj))
        }
    }
    # Habitats <- spTransform(Habitats,CRS(proj))
    # Breeding <- spTransform(Breeding,CRS(proj))

    #### create habitat grid ####
    bb <- bbox(EEZ)
    cs <- c(cell_size,cell_size)  # cell size
    cc <- bb[, 1] + (cs/2)  # cell offset
    cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
    grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)

    sp_grd <- SpatialGridDataFrame(grd,
                                   data=data.frame(id=1:prod(cd)),
                                   proj4string=CRS(proj4string(EEZ)))

    #### make grid into polygon ####
    library(Grid2Polygons)
    p <- Grid2Polygons(sp_grd)
    proj4string(p) <- CRS(proj)

    #### trim grid polygon to EEZ
    library(rgeos)
    p <- p[apply(gIntersects(EEZ,p,byid=T),1,any),]
    spChFIDs(p) <- paste0("c",1:length(p))
    p@data <- data.frame(cell_ID=paste0("c",1:length(p)))

    #### set area overlaps
    if(!any(is.na(areas))){
        for(i in seq_along(areas)){
            areas[[i]] <- apply(gIntersects(areas[[i]],p,byid=T),1,any)
            p@data <- data.frame(p@data,areas[[i]])
        }
        names(p@data) <- c("cell_ID",names(areas))
    }
    # p$Habitats <- apply(gIntersects(Habitats,p,byid=T),1,any)
    # p$Breeding <- apply(gIntersects(Breeding,p,byid=T),1,any)
    return(p)
}
