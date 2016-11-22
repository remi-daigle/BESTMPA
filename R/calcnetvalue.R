#' Calculate net value
#'
#' Calculates the net value of the model output (This function cannot work without model output). Uses the \code{Status_quo_profitability} ratio and the weighted mean of the distance travelled to get a fish (weighted by the catch gross value), and the \code{fish_operating_cost_ratio} to determine the relative profitability of scenarios
#'
#' @param catch_summary A merged dataframe of all the 'kg' output files
#' @param Status_quo_profitability The ratio of the landed value of the catch to the total operating costs for the Mixed Fishery
#' @param fish_operating_cost_ratio The ratio of operating cost that are variable with distance (labour and fuel) to the total operating costs for the Mixed Fishery
#'
#' @return
#' @export
#'
#' @examples
#' filenames <- list.files(results_folder,pattern="kg_")
#' catch_summary <- rbindlist(lapply(filenames,processkg,results_folder,distance,fish_landed_value))
#' catch_summary <- calcnetvalue(catch_summary,Status_quo_profitability,fish_operating_cost_ratio)
calcnetvalue <- function(catch_summary,Status_quo_profitability,fish_operating_cost_ratio){
    SQinitialstats <- catch_summary %>%
        filter(starts_with("SQ",vars=catch_summary$scenario),year==min(time)) %>%
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
