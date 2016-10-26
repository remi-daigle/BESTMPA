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
processfish <- function(fn,results_folder){
    fish <- fread(paste0(results_folder,'/',fn),drop=1)

    underscores <- grep("_",unlist(strsplit(fn,"")))
    start <- underscores[length(underscores)-2]+1

    nam <- as.character(unlist(strsplit(substr(fn,start,nchar(fn)-4),split = "_")))

    kgtable <- data.frame(scenario=nam[1],
                          replicate=nam[2],
                          year=as.numeric(nam[3]),
                          biomass=sum(fish2weight(fish,0:(ncol(fish)-1))))

                          return(kgtable)

}
