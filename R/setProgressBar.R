#' Set progress bar
#'
#' @description OS agnostic wrapper function for utils::setWinProgressBar and tcltk::setTkProgressBar
#'
#' @param title
#' @param pb
#' @param value
#' @param label
#' @inheritParams utils::setWinProgressBar
#'
#' @return
#' @export
#' @import tcltk
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
