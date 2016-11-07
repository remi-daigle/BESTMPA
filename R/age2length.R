#' Convert fish age to length
#'
#' Converts fish age to length using Von Bertalanffy growth model parameters (Knickle and Rose 2013)
#' Lt = Linf * {1-exp(-k*(t-t0))}, where Lt is length (cm) at age t (year), Linf is the asymptotic length (cm), k is the VB growth coefficient (1/year), and t0 is the x intercept (year). Linf = 112.03 (95% CI = 10.46). k = 0.13 (95% CI = 0.021). t0 = 0.18).
#'
#' @param t The age of the fish (year)
#' @param Linf_mean Von Bertalanffy growth model parameter - mean asymptotic length (cm)
#' @param Linf_SD Von Bertalanffy growth model parameter - standard deviation asymptotic length (cm)
#' @param k_mean Von Bertalanffy growth model parameters - mean growth coefficient (1/year)
#' @param k_SD Von Bertalanffy growth model parameters - standard deviation growth coefficient (1/year)
#' @param t0 Von Bertalanffy growth model parameters - x intercept (year)
#'
#' @return
#' @export
#'
#' @examples
#' # one t at a time
#' age2length(t = 1, Linf_mean = 112.03, Linf_SD = 10.46/1.96,k_mean = 0.13, k_SD = 0.021/196, t0 = 0.18)
#'
#' # multiple t at a time
#' age2length(t = 0:50, Linf_mean = 112.03, Linf_SD = 10.46/1.96,k_mean = 0.13, k_SD = 0.021/196, t0 = 0.18)
age2length <- function(t=0:50,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18){
        Linf <- rnorm(length(t),Linf_mean,Linf_SD)
        k <- rnorm(length(t),k_mean,k_SD)
        Lt  <-  Linf * (1-exp((-k)*(t-t0)))
        index <- Lt<0
        if(any(index)) Lt[index] <- 0
        return(Lt)
}
