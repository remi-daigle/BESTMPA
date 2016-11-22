#' Title
#'
#' @param fn
#' @param results_folder
#' @param distance
#' @param fish_landed_value
#'
#' @return
#' @export
#'
#' @examples
processkg <- function(fn,results_folder,distance,fish_landed_value){
    kgs <- as.matrix(fread(paste0(results_folder,'/',fn)[1],
                          col.names=c("cell",paste("community_",dimnames(distance)[[2]]))) %>%
                        dplyr::select(starts_with("community")))
    # remove suffix
    underscores <- grep("_",unlist(strsplit(fn,"")))
    suffix <- NA
    if(length(underscores)>4){
        suffix <- substr(fn,underscores[5]+1,nchar(fn)-4)
        fn <- gsub(paste0("_",suffix),"",fn)
        underscores <- grep("_",unlist(strsplit(fn,"")))

    }
    start <- underscores[length(underscores)-2]+1

    nam <- as.character(unlist(strsplit(substr(fn,start,nchar(fn)-4),split = "_")))

    kgtable <- data.frame(scenario=nam[1],
                          replicate=nam[2],
                          year=as.numeric(nam[3]),
                          kg=sum(kgs),
                          grossvalue=sum(kgs)*fish_landed_value,
                          distanceGV=weighted.mean(as.vector(distance),(as.vector(kgs)*fish_landed_value)))

    if(!is.na(suffix)) kgtable$scenario <- paste0(kgtable$scenario,"_",suffix)

    return(kgtable)

}
