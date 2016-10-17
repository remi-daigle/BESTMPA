#' Title
#'
#' @param env
#' @param fig
#' @param results_folder
#' @param delete
#' @param memorylimit
#'
#' @return
#' @export
#'
#' @examples
housekeeping <- function(results_folder="output",env=FALSE,fig=FALSE,delete=FALSE,memorylimit=memory.limit()){

    if(length(dev.list()["RStudioGD"])>1&fig) dev.off(dev.list()["RStudioGD"])

    if(delete) unlink(results_folder,recursive = T,force=T)

    if(!dir.exists(results_folder)) dir.create(results_folder)

    if(memorylimit>memory.limit()) memory.limit(size=memorylimit)

    tmp <- ls(envir=globalenv())

    tmp <- tmp[tmp!="results_folder"]

    if(length(tmp)>0) rm(list=tmp,envir=globalenv())
}