#' Title
#'
#' @param domain
#' @param gr
#' @param dist
#' @param priority
#' @param prioritize
#' @param excluded
#' @param tol
#' @param i
#'
#' @return
#' @export
#'
#' @examples
selectmpa <- function(domain,gr,dist,priority,prioritize,excluded,name,tol,i){
    if(tol<1/length(domain)){
        candidates <- 1:length(domain)
    } else {
        candidates <- unique(unlist(neighborhood(gr,order=dist/tol,node=which(as.logical(domain@data[,names(domain)==paste0(name,"_",i)])))))

        # remove non-priority and excluded
        if(!any(is.na(priority))&prioritize) candidates <- candidates[priority[candidates]]
        if(!any(is.na(excluded))) candidates <- candidates[!excluded[candidates]]

        candidates <- candidates[!candidates %in% unlist(neighborhood(gr,order=dist*tol,node=which(as.logical(domain@data[,names(domain)==paste0(name,"_",i)]))))]
    }



    return(candidates)

}
