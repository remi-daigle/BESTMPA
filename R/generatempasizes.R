#' Title
#'
#' @param sizes
#' @param cell_size
#' @param p
#' @param MPA_coverage
#' @param n
#'
#' @return
#' @export
#'
#' @examples
generatempasizes <- function(cell_size,p,MPA_coverage,Status_quo){
        # MPA size frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
        MPAs_mar_REP_M_AREA <- read.csv("data/MPAs_mar_REP_M_AREA.csv")

        # remove MPA's smaller than grid size and larger than entire protected area
        MPAs_mar_REP_M_AREA <- MPAs_mar_REP_M_AREA/cell_size^2
        MPAs_mar_REP_M_AREA <- round(log10(MPAs_mar_REP_M_AREA[MPAs_mar_REP_M_AREA>=1&MPAs_mar_REP_M_AREA<length(p)*MPA_coverage]),2)

        # make probability table
        prob_table <- as.data.frame(table(MPAs_mar_REP_M_AREA),stringsAsFactors=FALSE)
        prob_table$prob <- prob_table$Freq/sum(prob_table$Freq)
        prob_table$cumsum <- cumsum(prob_table$prob)

        # generate MPA sizes (in # of cells) to use
        MPA_sizes <- round(10^as.numeric(apply(matrix(runif(length(p))),1,function(x) prob_table$MPAs_mar_REP_M_AREA[
            sum((prob_table$cumsum-x)<0)+1
            ])))
        # clip to MPA coverage
        MPA_sizes <- MPA_sizes[cumsum(MPA_sizes)<(length(p)*MPA_coverage-sum(Status_quo))]
}
