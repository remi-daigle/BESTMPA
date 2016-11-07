#' Prepare environment and file structure
#'
#' @description Prepares the environment and your file structure for modelling. This function will conveniently create a results folder, clear the environment, figures, delete previous results, and increase the memory limit if desired.
#'
#' @param env (logical) If TRUE, clears the environment
#' @param fig (logical) If TRUE, clears the figures
#' @param results_folder
#' @param delete (logical) If TRUE, deletes the all the files in the results_folder. WARNING: files are permanently deleted, they will not be in your "recycling bin". Only use TRUE if you are certain you want to delete the files
#' @param memorylimit increases the limit in force on the total memory allocation for Windows machines.
#'
#' @return NA
#' @export
#'
#' @examples
#' housekeeping(results_folder=results_folder,env=TRUE,fig=TRUE,delete=FALSE,memorylimit=16304)
housekeeping <- function(results_folder="output",env=FALSE,fig=FALSE,delete=FALSE,memorylimit=16304){

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

    if(.Platform$OS.type=="windows"){
        if(memorylimit>memory.limit()) memory.limit(size=memorylimit)
    }

    tmp <- ls(envir=globalenv())

    tmp <- tmp[tmp!="results_folder"&tmp!="newrun"]

    if(length(tmp)>0&env) rm(list=tmp,envir=globalenv())
}
