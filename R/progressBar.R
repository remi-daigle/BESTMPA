#' Create progress bar
#'
#' @description OS agnostic wrapper function for utils::winProgressBar and tcltk::tkProgressBar
#'
#' @param title,label
#' @param min,max
#' @param initial,value
#' @param width
#' @inheritParams utils::winProgressBar
#'
#' @return
#' @export
#' @import tcltk
#'
#' @examples
progressBar <- function(title = "R progress bar", label = "",
                        min = 0, max = 1, initial = 0, width = 300){
    if(.Platform$OS.type=="windows"){
        pb <- winProgressBar(title,label,min,max,initial,width)
    } else {
        pb <- tkProgressBar(title,label,min,max,initial,width)
    }
    return(pb)
}
