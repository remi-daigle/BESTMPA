#' Title
#'
#' @param recruits
#' @param cml
#' @param lM
#' @param CCs
#'
#' @return
#' @export
#'
#' @examples
larvaldispersal <- function(recruits,cml,lM,CCs){
    recruits <- dispersal(recruits,cm=cml,ages=0)

    recruits <- mortality(recruits,M=sample(lM,length(recruits)),ages=0)

    # density dependent recruitment
    recruits <- roundprob(recruits/(1+rowSums(fish)/CCs))
    recruits[is.nan(recruits)] <- 0
    return(recruits)
}
