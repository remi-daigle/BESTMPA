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

    if(delete){
        if(interactive()){
            answer <- readline(paste('do you really want to delete all the files in',results_folder,'(y/n):'))
            if(answer=='y') unlink(results_folder,recursive = T,force=T)
        } else {
            unlink(results_folder,recursive = T,force=T)
        }

    }

    Sys.sleep(0.1)
    if(!dir.exists(results_folder)) dir.create(results_folder)

    if(memorylimit>memory.limit()) memory.limit(size=memorylimit)

    tmp <- ls(envir=globalenv())

    tmp <- tmp[tmp!="results_folder"]

    if(length(tmp)>0&env) rm(list=tmp,envir=globalenv())
}
