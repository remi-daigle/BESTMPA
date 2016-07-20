# housekeeping
rm(list=ls())
if(length(dev.list()["RStudioGD"])>1) dev.off(dev.list()["RStudioGD"])
# specifiy folder for results
results_folder <- "D:/BESTMPA_results/"
if(!dir.exists(results_folder)) dir.create(results_folder)
# memory.limit(32608)

#### user inputs ####
source("user_input.R")

# #### source custom functions ####
# source("functions.R")

# #### Spatial base layer ####
# # Basic grid
# p <- initgrid(EEZ,Habitats,Breeding,cell_size,proj)
#
# #### set status-quo scenario ####
# p <- addscenario(p,oldMPA,replicates=replicates,name="Status_quo")
#
# #### set maximum distance ####
# p <- addscenario(p,oldMPA,MPA_coverage=MPA_coverage,replicates=replicates,name="MPA_MD",cell_size=cell_size)
#
# #### set fixed distance ####
# p <- addscenario(p,oldMPA,MPA_coverage=MPA_coverage,replicates=replicates,dist=75000,name="MPA_FX",cell_size=cell_size)
#
# #### set targeted scenario ####
# p <- addscenario(p,oldMPA,MPA_coverage=MPA_coverage,replicates=replicates,dist=75000,name="MPA_TR",Habitats=TRUE,Breeding=TRUE,cell_size)
# writeOGR(p,dsn="shapefiles",layer="master_polygon",driver="ESRI Shapefile",overwrite_layer = TRUE)
p <- readOGR(dsn="shapefiles",layer="master_polygon")

# load connectivity matrix
#adult
cma <- initcm(p,e_fold_adult,cell_size)
#larvae
cml <- initcm(p,e_fold_larvae,cell_size)

# calculate distances
distance <- gDistance(spTransform(fish_communities,proj),p,byid = T)

for(s in names(p)[c(4,154)]){
    for(y in tot_time){
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
        if(any(fish<0)) browser()

        # dispersal
        fish <- dispersal(fish,cm=cma,ages=5:50)

        # mortality
        fish <- mortality(fish,M=sample(M,nrow(fish)),ages=1:50)

        # fish outside the habitat die automatically
        fish <- fish*p$Habitats

        # reproduction and larval dispersal
        print("larvae")

        recruits <- fish2eggs(fish,fecundity=0.5*10^6,age_mat_steepness=2.5,age_mat_sigmoid=4,l_to_w_int=0.000011,l_to_w_power=2.91,Linf_mean=112.03,Linf_SD=10.46/1.96,k_mean=0.13,k_SD=0.021/196,t0=0.18)

        recruits <- dispersal(recruits,cm=cml,ages=0)

        recruits <- mortality(recruits,M=sample(lM,length(recruits)),ages=0)

        # density dependent recruitment
        recruits <- recruits/(1+rowSums(fish)/CCs)

        # fishing
        print("fishing")
        ages=4:50
        quota <- sum(apply(fish[,ages+1],1,function(x) sum(length2weight(age2length(ages))*x)))/3
        print(quota)

        if(y %in% time){
            #spinup using status quo
            fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=p$MPA_SQ_1)
        } else {
            #apply MPAs
            fishcatch <- fishing(fish,quota,ages,distance,fish_licenses,mpa=p[[s]])
        }
        fish <- fish-fishcatch

        # writing to disk
        print("writing")

        write.csv(fish,paste0(results_folder,"fish_",s,"_",y,".csv"))
        write.csv(fishcatch,paste0(results_folder,"fishcatch_",s,"_",y,".csv"))
        print(paste(s,"for year",y,"complete"))

    }
}

