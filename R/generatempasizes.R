#' Title
#'
#' @param cell_size
#' @param p
#' @param cells
#' @param MPA_coverage
#' @param included
#' @param MPAs_AREA Vector of MPA size frequency in m^2. Defaults to MPA szie frequency obtained from WDPA database downloaded from http://www.protectedplanet.net/search?marine=1 in March 2015
#'
#' @return
#' @export
#'
#' @examples
generatempasizes <- function(cell_size,cells=1,p,MPA_coverage,included,MPAs_AREA=MPAs_mar_REP_M_AREA){
        # remove MPA's smaller than grid size and larger than entire protected area
        MPAs_mar_REP_M_AREA <- MPAs_mar_REP_M_AREA/cell_size^2*cells
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
        MPA_sizes <- MPA_sizes[cumsum(MPA_sizes)<(length(p)*MPA_coverage-sum(included))]
        return(MPA_sizes)
}
