#' Title
#'
#' @param fish
#' @param ages
#'
#' @return
#' @export
#'
#' @examples
fish2weight <- function(fish,ages){
    apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
}
