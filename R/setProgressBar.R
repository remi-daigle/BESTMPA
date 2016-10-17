#' Title
#'
#' @param title
#' @param pb
#' @param value
#' @param label
#'
#' @return
#' @export
#'
#' @examples
setProgressBar <- function(pb, value, title = NULL, label = NULL){
    if(.Platform$OS.type=="windows"){
        pb <- setWinProgressBar(pb, value, title, label)
    } else {
        pb <- setTkProgressBar(pb, value, title, label)
    }
    return(pb)
}
