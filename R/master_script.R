cell_size <- 20
maxage=50
cells=5000
distance=round(matrix(runif(cells*5,0,200),ncol=5)/cell_size)
fish_landed_value <- 1.24
fish_licenses <- c(866, 4714, 3002, 879, 963)
ymax=20

######################### Sources of natural Mortality ##################################
# natural mortality (Swain & Chouinard 2008)
M <- rnorm(10000,0.5938,0.0517)
M <- M[M<=1&M>=0] # eliminate any possible values of M >1 or M <0

# larval mortality (Mountain et al. 2008)
lM <- rbeta(10000,1000,1.2) #larval mortality of 99.88% (range 98.98-99.99%)
#hist(lM);mean(lM);min(lM);max(lM)

# Beverton-Holt model for carrying capacity based recruitment mortality, carrying capacity is the mean of North American carrying capacities in Table 3 of Myers et al. 2001 (CC=0.431088 tonnes/km^2 SD=0.386696)
CC <- 0.579352312*1000                 #(kg/km^2)
CC_SD <- 0.772463562*1000              #(kg/km^2)
#if this CC was fixed, total biomass would not exceed 500,000 t

# Habitat carrying capacity, in kg of virtual fish per cell (4548 is the number of cells in the habitat grid). This could be substituted with "known" habitat carrying capacity.
CCs <- rnorm(100000,CC,CC_SD)*(cell_size^2)
CCs <- CCs[CCs>0][1:10] #enforce no negative CCs

##############################################################################





for(y in 1:ymax){
    # adult disersal & mortality
    print("adults")
    if(y==1){
        fish <- initpop(initial_abun=250*10^6,cells=cells,maxage=50,rate=0.7)
    } else {
        fish[,2:maxage] <- fish[,1:(maxage-1)]
        fish[,1] <- recruits
    }

    if(any(fish<0)) browser()

    cm <- matrix(1/nrow(fish),nrow=nrow(fish),ncol=nrow(fish))

    fish <- dispersal(fish,cm,ages=5:50)

    fish <- mortality(fish,M=sample(M,nrow(fish)))
    # reproduction and larval dispersal
    print("larvae")

    recruits <- fish2eggs(fish,fecundity=0.5*10^6,age_mat_steepness=2.5,age_mat_sigmoid=4,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18)

    recruits <- dispersal(recruits,cm,ages=0)

    recruits <- mortality(recruits,M=sample(lM,length(recruits)),ages=0)
    # recruitment
    recruits <- recruits/(1+rowSums(fish)/CCs)

    # fishing
    print("fishing")
    ages=4:50
    quota <- sum(apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x)))/3
    haul <- quota/sum(fish_licenses)
    print(quota)

    fishcatch <- fishing(fish,quota,ages,distance,haul)
    fish <- fish-fishcatch

    # writing to disk
    print("writing")

    write.csv(fish,paste0("fish_",y,".csv"))
    write.csv(fishcatch,paste0("fishcatch_",y,".csv"))
    print(paste("year",y,"complete"))

}
