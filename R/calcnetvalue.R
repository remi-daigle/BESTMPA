#' Title
#'
#' @param catch_summary
#' @param Status_quo_profitability
#' @param fish_operating_cost_ratio
#'
#' @return
#' @export
#'
#' @examples
calcnetvalue <- function(catch_summary,Status_quo_profitability,fish_operating_cost_ratio){
    SQinitialstats <- catch_summary %>%
        filter(scenario=="SQ",year==min(time)) %>%
        summarize(distanceGV=mean(distanceGV,na.rm=T),
                  grossvalue=mean(grossvalue,na.rm=T))

    fleetfixedcost <- SQinitialstats$grossvalue/Status_quo_profitability*(1-fish_operating_cost_ratio)
    fleetvariablecost <- SQinitialstats$grossvalue/Status_quo_profitability*fish_operating_cost_ratio/SQinitialstats$distanceGV

    catch_summary$netvalue <- catch_summary$grossvalue-fleetfixedcost-(fleetvariablecost*catch_summary$distanceGV)
    catch_summary$cumsum <- do.call(c, tapply(catch_summary$netvalue,
                                              paste0(catch_summary$scenario, catch_summary$replicate),
                                              FUN=cumsum))
    return(catch_summary)
}
