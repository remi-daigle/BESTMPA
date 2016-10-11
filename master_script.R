# housekeeping
rm(list=ls())
if(length(dev.list()["RStudioGD"])>1) dev.off(dev.list()["RStudioGD"])
# specifiy folder for results
results_folder <- "D:/BESTMPA_results"
unlink(results_folder,recursive = T,force=T)
if(!dir.exists(results_folder)) dir.create(results_folder)

# memory.limit(32608)

require(BESTMPA)
#### user inputs ####
source("user_input.R")



#### Spatial base layer ####
cells=3
# Basic grid
p <- initgrid(EEZ,cell_size,proj,areas=c(Habitats=Habitats,Breeding=Breeding))

#### set status-quo scenario ####
p <- addscenario(domain=p,included=oldMPA,replicates=replicates,name="Status_quo",cells=cells)

#### set maximum distance ####
p <- addscenario(domain=p,included=oldMPA,MPA_coverage=MPA_coverage,replicates=replicates,name="MPA_MD",cell_size=cell_size,cells=cells)

#### set fixed distance ####
p <- addscenario(domain=p,included=oldMPA,MPA_coverage=MPA_coverage,replicates=replicates,dist=75000,name="MPA_FX",cell_size=cell_size,cells=cells)

#### set targeted scenario ####
p <- addscenario(domain=p,included=oldMPA,priority=p$Habitats,excluded=p$Breeding,MPA_coverage=MPA_coverage,replicates=replicates,dist=75000,name="MPA_TR",cell_size,cells=cells)


writeOGR(p,dsn="shapefiles",layer="master_polygon",driver="ESRI Shapefile",overwrite_layer = TRUE)
p <- readOGR(dsn="shapefiles",layer="master_polygon")

plot(p);plot(p[p$MPA_TR_1==1,],col='blue',add=T)

# adjust CCs to be 0 outside habitat
CCs <- CCs*p$Habitats

# load connectivity matrix
#adult
cma <- initcm(p,e_fold_adult,cell_size)
#larvae
cml <- initcm(p,e_fold_larvae,cell_size)

# calculate distances
distance <- gDistance(spTransform(fish_communities,proj),p,byid = T)
breeding_near <- apply(gDistance(spTransform(Breeding,proj),p,byid = T),1,which.min)

# grep("MPA_",names(p))
scenarios <- names(p)[grep("MPA_",names(p))]
scenarios <- scenarios[order(as.numeric(substr(scenarios,8,nchar(scenarios))))]
for(s in scenarios){
    for(y in tot_time){
        gc()
        # adult disersal & mortality
        print("adults")
        if(y==min(tot_time)){
            fish <- initpop(initial_abun=250*10^6,cells=length(p),maxage=maxage,rate=0.7)
            biomass_est_bank <- NULL
        } else {
            fish[,2:maxage] <- fish[,1:(maxage-1)]
            fish[,1] <- recruits
        }

        # check for extinctions
        if(any(fish<0)) fish[fish<0] <- 0

        # dispersal
        fish <- dispersal(fish,cm=cma,ages=5:maxage)

        # mortality
        fish <- mortality(fish,M=sample(M,nrow(fish)),ages=1:maxage)

        # fish outside the habitat die automatically
        fish <- fish*p$Habitats

        # reproduction
        print("larvae")

        recruits <- fish2eggs(fish,fecundity=0.5*10^6,age_mat_steepness=2.5,age_mat_sigmoid=4,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18)

        # relocate eggs to nearest breeding ground
        recruits <- data.frame(r=recruits,b=breeding_near,polys=p$Breeding*breeding_near!=0) %>%
            group_by(b) %>%
            summarize(rec=sum(r),poly=sum(polys)) %>%
            mutate(recruits=rec/poly) %>%
            ungroup() %>%
            roundprob()
        recruits <- (left_join(data.frame(b=breeding_near),recruits,by="b")*p$Breeding) %>%
            dplyr::select(recruits) %>%
            unlist() %>%
            matrix()


        #larval dispersal
        recruits <- dispersal(recruits,cm=cml,ages=0)

        recruits <- mortality(recruits,M=sample(lM,length(recruits)),ages=0)

        # density dependent recruitment
        recruits <- roundprob(recruits/(1+rowSums(fish)/CCs))
        recruits[is.nan(recruits)] <- 0

        # fishing
        print("fishing")
        ages=4:maxage


        # if(!y %in% time){
            #spinup using status quo
        # if(!y %in% time) biomass <- sum(apply(fish[,ages+1]*(1-p$MPA_SQ_1),1,function(x) sum(length2weight(age2length(ages))*x)))
        # if(y %in% time) biomass <- sum(apply(fish[,ages+1]*(1-p[[s]]),1,function(x) sum(length2weight(age2length(ages))*x)))

        biomass <- sum(apply(fish,1,function(x) sum(length2weight(age2length(ages))*x)))
        # if(!y %in% time) biomass <- apply(fish[,ages+1]*(1-p$MPA_SQ_1),1,function(x) sum(length2weight(age2length(ages))*x))
        # if(y %in% time) biomass <- apply(fish[,ages+1]*(1-p[[s]]),1,function(x) sum(length2weight(age2length(ages))*x))


            biomass_est_bank <- c(rnorm(1,biomass,biomass*0),biomass_est_bank)
        # biomass_est_bank <- c(mean(sample(biomass,length(biomass)*sampling_pop))*length(biomass),
                              # biomass_est_bank)

            if(length(biomass_est_bank)>biomass_est_n_years) biomass_est_bank <- biomass_est_bank[1:biomass_est_n_years]
            x <- 1:length(biomass_est_bank)

            quota <- suppressWarnings(as.numeric(predict(lm(biomass_est_bank ~ x), data.frame(x = length(biomass_est_bank)+1))))*FMSY*FMSY_buffer
            print(quota)

            if(!y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=p$MPA_SQ_1)
            if(y %in% time) fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=p[[s]])


        # } else {
        #     #apply MPAs
        #     biomass <- sum(apply(fish[,ages+1]*(1-p[[s]]),1,function(x) sum(length2weight(age2length(ages))*x)))*FMSY#*FMSY_buffer
        #     biomass_est_bank <- c(rnorm(1,biomass,biomass*0.25),biomass_est_bank)
        #     if(length(biomass_est_bank)>biomass_est_n_years) biomass_est_bank <- biomass_est_bank[1:biomass_est_n_years]
        #     quota <- mean(biomass_est_bank)
        #
        #     print(quota)
        #     fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=p[[s]])
        # }
            catch <- Reduce("+",fishcatch)
            print(paste("caught ",sum(catch*p[[s]])," fish in MPAs (or future MPAs)"))
        fish <- fish-catch
        kg <- t(do.call(rbind,lapply(fishcatch,fish2weight,ages)))
        names(kg) <- names(fish_communities)

        # plot(p[p$Habitats==1,],main=y)
        # plot(p[kg[,2]>0,],add=TRUE,col='blue')
        # plot(spTransform(fish_communities,proj)[2],add=T,col='lightblue')


        # writing to disk
        print("writing")

        if(y %in% time) write.csv(fish,paste0(results_folder,"/fish_",s,"_",y,".csv"))
        if(y %in% time) write.csv(fishcatch,paste0(results_folder,"/fishcatch_",s,"_",y,".csv"))
        if(y %in% time) write.csv(kg,paste0(results_folder,"/kg_",s,"_",y,".csv"))
        print(paste(s,"for year",y,"complete"))

    }
}

library(tidyverse)
library(data.table)
library(ggplot2)
library(BESTMPA)
# memory.limit(32608)
#
# # load fish stocks
# filenames <- list.files(results_folder,pattern="fish_")
#
# # fish <- lapply(paste0(results_folder,filenames), fread) %>%
# #     setNames(filenames) %>%
# #     rbindlist(use.names=TRUE,idcol=TRUE) %>%
# #     separate(col=.id,into=c("fish","treatment1","treatment2","replicate","year","csv")) %>%
# #     unite(scenario,treatment1,treatment2) %>%
# #     select(-fish,-csv) %>%
# #     ungroup() %>%
# #     as.data.frame()
#
# # load fish catches
# filenames <- list.files(results_folder,pattern="fishcatch_")
# filenames <- filenames[c(grep('SQ',filenames),grep('TR',filenames))]
# fileinfo <- substr(filenames,15,nchar(filenames)-4)
#
# catch <- lapply(paste0(results_folder,filenames), fread) %>%
#     setNames(fileinfo) %>%
#     rbindlist(use.names=TRUE,idcol=TRUE) %>%
#     separate(col=.id,into=c("scenario","replicate","year")) %>%
#     ungroup()
#
# # calculate costs
# # fish$biomass <- apply(select(fish,starts_with('age'))[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))
#
# catch$biomass <- apply(dplyr::select(catch,starts_with('age')),1,function(x) sum(length2weight(age2length(1:maxage))*x))
#
# catch$grossvalue <- catch$biomass*fish_landed_value
#
# catch$distancekm <- apply(distance,1,min)/1000
#
# catch_summary <- catch %>%
#     filter(replicate<6) %>%
#     group_by(scenario,replicate,year) %>%
#     summarise(distanceGV=weighted.mean(distancekm,grossvalue),
#               grossvalue=sum(grossvalue)) %>%
#     ungroup()
#
# SQinitialstats <- catch_summary %>%
#     filter(scenario=="SQ",year==min(time)) %>%
#     summarize(distanceGV=mean(distanceGV,na.rm=T),
#               grossvalue=mean(grossvalue,na.rm=T))
#
# fleetfixedcost <- SQinitialstats$grossvalue/Status_quo_profitability*(1-fish_operating_cost_ratio)
# fleetvariablecost <- SQinitialstats$grossvalue/Status_quo_profitability*fish_operating_cost_ratio/SQinitialstats$distanceGV
#
# catch_summary$netvalue <- catch_summary$grossvalue-fleetfixedcost-(fleetvariablecost*catch_summary$distanceGV)
# catch_summary$cumsum <- do.call(c, tapply(catch_summary$netvalue,
#                                           paste0(catch_summary$scenario, catch_summary$replicate),
#                                           FUN=cumsum))
#
#
#
# ggplot(catch_summary,aes(x=as.numeric(year),y=netvalue,colour=scenario))+
#     geom_point(cex=2)+geom_smooth()+
#     theme_classic()
#
# ggplot(catch_summary,aes(x=as.numeric(year),y=cumsum,colour=scenario))+
#     geom_point(cex=2)+geom_smooth()+
#     theme_classic()
#
#
# library(data.table)
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(BESTMPA)
# memory.limit(32608)


# load fish catches
filenames <- list.files(results_folder,pattern="kg_")
# filenames <- filenames[c(grep('SQ',filenames),grep('TR',filenames))]
fileinfo <- substr(filenames,8,nchar(filenames)-4)

catch <- lapply(paste0(results_folder,'/',filenames), fread, col.names=c("cell",paste("community_",names(fish_communities)))) %>%
    setNames(fileinfo) %>%
    rbindlist(use.names=TRUE,idcol=TRUE) %>%
    separate(col=.id,into=c("scenario","replicate","year")) %>%
    gather(community,kg,starts_with("community_")) %>%
    ungroup()

# any(is.na(catch$grossvalue))

# calculate costs
# fish$biomass <- apply(select(fish,starts_with('age'))[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x))

# catch$biomass <- apply(dplyr::select(catch,starts_with('age')),1,function(x) sum(length2weight(age2length(1:maxage))*x))

catch$grossvalue <- catch$kg*fish_landed_value

catch$distancekm <- as.vector(distance)/1000

catch_summary <- catch %>%
    group_by(scenario,replicate,year) %>%
    summarise(distanceGV=weighted.mean(distancekm,grossvalue),
              grossvalue=sum(grossvalue)) %>%
    ungroup()

catch_summary$distanceGV[is.nan(catch_summary$distanceGV)] <- 0

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



ggplot(catch_summary,aes(x=as.numeric(year),y=grossvalue,colour=scenario))+
    geom_point(cex=2)+geom_smooth()+
    theme_classic()

ggplot(catch_summary,aes(x=as.numeric(year),y=cumsum,colour=scenario))+
    geom_point(cex=2)+geom_smooth()+
    theme_classic()

ggplot(catch_summary,aes(x=as.numeric(year),y=distanceGV,colour=scenario))+
    geom_point(cex=2)+geom_smooth()+
    theme_classic()
