#' Title
#'
#' @param fish
#' @param maxage
#' @param biomass_est_bank
#'
#' @return
#' @export
#'
#' @examples
#'
estimatequota <- function(fish,maxage,biomass_est_bank,y,tot_time,FMSY,FMSY_buffer){
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
