#' Title
#'
#' @param title
#' @param label
#' @param min
#' @param max
#' @param initial
#' @param width
#'
#' @return
#' @export
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
