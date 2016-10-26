#' Title
#'
#' @param t
#' @param value
#' @param i
#'
#' @return
#' @export
#'
#' @examples
SDRvalues <- function(catch_summary,t,value,SDR){
    # browser()
    for(sdr in SDR){
        nam <- c(names(catch_summary),paste0('SDR = ',sdr),paste0('SDRcumsum = ',sdr))
        t0 <- min(t)
        catch_summary$SDR <- value*(1/(1+sdr)^(t-t0))
        catch_summary <- catch_summary %>%
            group_by(scenario,replicate) %>%
            mutate(SDRcumsum=cumsum(SDR)) %>%
            ungroup()
        names(catch_summary) <- nam
    }

    return(catch_summary)
}
