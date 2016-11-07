#' Estimating the quota
#'
#' Returns the fishing quota given the biomass in \code{fish} using Fisheries mortality at Maximum Sustainable Yield with a precautionary buffer. Uses a linear projection of the last 5 years (if available; stored in the cache)
#'
#' @param fish
#' @param maxage Maximum age considered in \code{fish} matrix
#' @param y current year
#' @param tot_time The time over which the model is run including spin-up time
#' @param FMSY Fisheries mortality at Maximum Sustainable Yield
#' @param FMSY_buffer quota set to fraction of FMSY as per precautionary principle
#' @inheritParams reproduction
#' @return
#' @export
#'
#' @examples
#' # create a fish matrix
#' fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
#' # quota
#' quota <- estimatequota(fish,maxage=20,y=2001,tot_time=2001:2071,FMSY=0.28,FMSY_buffer=0.667)

estimatequota <- function(fish,maxage,y,tot_time,FMSY,FMSY_buffer){
    if(y==min(tot_time)){
        assign('biomass_est_bank',NULL,envir=.BESTMPAcacheEnv)
    }


    biomass <- sum(apply(fish,1,function(x) sum(length2weight(age2length(0:maxage))*x)))


    biomass_est_bank <- get("biomass_est_bank",envir=.BESTMPAcacheEnv)
    biomass_est_bank <- c(rnorm(1,biomass,biomass*0.2),biomass_est_bank)

    if(length(biomass_est_bank)>biomass_est_n_years) biomass_est_bank <- biomass_est_bank[1:biomass_est_n_years]
    x <- 1:length(biomass_est_bank)

    quota <- suppressWarnings(as.numeric(predict(lm(biomass_est_bank ~ x), data.frame(x = length(biomass_est_bank)+1))))*FMSY*FMSY_buffer
}
