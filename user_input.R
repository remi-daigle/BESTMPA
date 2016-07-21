#################################### User input ##############################################
##
############################## Basic model parameters ########################################
# total model run time in years (e.g. 2001:2100 would be 100 years)
time <- 2001:2051
spinup <- 10 # number of years before "time" the model starts, results from spin-up years are not saved, all scenarios start as status quo
tot_time <- (min(time)-spinup):max(time)

# # time step in years
# dt <- 1

# replicates (should be more than 1 or fish_value.R onwards will not work)
replicates <- c(1:50)

# cell size in m
cell_size <- 20000

# default projection
proj  <- "+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# create new protection scenarios? (TRUE creates new maps, but is slow. FALSE uses previously saved maps)
# very computationally expensive if TRUE
protect_scen_new <- FALSE

# plot during loops?
time_loop_plot <- FALSE

# analysis mode (skips the loops if FALSE), or full model if TRUE
full_model  <- T

# use connectivity matrices or random dispersal? If FALSE, adults and/or larvae will disperse randomly, otherwise they will disperse according to the connectivity matrices (source polygon as row names, settlement polygon as column names)
adult_con_mat <- TRUE
larvae_con_mat <- TRUE

############################# fish growth and reproduction #######################################
# #Von Bertalanffy growth model parameters (Knickle and Rose 2013)
#Lt = Linf * {1-exp(-k*(t-t0))}, where Lt is length (cm) at age t (year), Linf is the asymptotic length (cm), k is the VB growth coefficient (1/year), and t0 is the x intercept (year). Linf = 112.03 (95% CI = 10.46). k = 0.13 (95% CI = 0.021). t0 = 0.18).
Linf_mean <- 112.03
Linf_SD <- 10.46/1.96
k_mean <- 0.13
k_SD <- 0.021/1.96
t0 <- 0.18

# calculate new weight - Length-weight relationship (Knickle and Rose 2013) fish$weight <- l_to_w_int * fish$length^l_to_w_power
l_to_w_int  <- 0.000011
l_to_w_power <- 2.91

# minimum age at maturity logistic equation
age_mat_steepness <- 2.5
age_mat_sigmoid <- 4


# Fecundity (size dependent). 0.5 million eggs per kg of female
fecundity <- 0.5*10^6

# Maximum age considered in matrix
maxage <- 50

######################### Sources of natural Mortality ##################################
# natural mortality (Swain & Chouinard 2008)
M <- rnorm(10000,0.5938,0.0517)
M <- M[M<=1&M>=0] # eliminate any possible values of M >1 or M <0

# larval mortality (Mountain et al. 2008)
lM <- rbeta(10000,1000,1.2) #larval mortality of 99.88% (range 98.98-99.99%)
#hist(lM);mean(lM);min(lM);max(lM)

# Beverton-Holt model for carrying capacity based recruitment mortality, carrying capacity is the mean of North American carrying capacities in Table 3 of Myers et al. 2001 (mean of log CC=-1.202222222 tonnes/km^2 SD=0.9199018667)

# Habitat carrying capacity, in kg of virtual fish per cell (4779 is the number of cells in the default grid). This could be substituted with "known" habitat carrying capacity.
CCs <- (10^rnorm(10000,-1.202222222,0.9199018667))*(cell_size^2)/1000
CCs <- CCs[CCs>0][1:4779] #enforce no negative CCs
########################### Dispersal ##################################################
# larval dispersal kernels are assumed to be exponential, e_fold_larvae is the e folding scale in km (the distance at which there will be fewer settlers by a factor of e). We assume that scale to be sqrt of 2cm/s*90d (avg current velocity * PLD) because we assume that the current is like a random walk
e_fold_larvae <- sqrt(2/100000*60*60*24*90)*1000

# adult dispersal kernels are also assumed to be exponential, e_fold_adult (in km) was calculated from data in Lawson & Rose 2000
e_fold_adult  <- 74.139*1000
# minimium size for adult migration (cm) Lawson & Rose 2000
min_size_migration <- 50

###################### Fisherman behaviour #############################################
# specify spatial distribution of fish_licenses for shore distance calculation in effort calculation
# can be spatial points or spatial polygons data frame (sp package)
require(raster)
require(rgeos)
fish_communities <- getData('GADM', country="CAN", level=1) # provinces
fish_communities <- fish_communities[fish_communities$NAME_1=="New Brunswick"|
                                         fish_communities$NAME_1=="Newfoundland and Labrador"|
                                         fish_communities$NAME_1=="Nova Scotia"|
                                         fish_communities$NAME_1=="Prince Edward Island"|
                                         fish_communities$ID_1==11,]    # this means Quebec, the accent make bad things happen when formatting
# fish_communities <- getBigPolys(gSimplify(fish_communities,tol=0.05,topologyPreserve = T))
fish_communities <-gSimplify(fish_communities,tol=0.05,topologyPreserve = T)

########################## Fisheries Management ########################################
# number of licenses per region in fish_communities
fish_licenses <- c(866, 4714, 3002, 879, 963)

# fisheries mortality at Maximum Sustainable Yield (Mountain et al. 2008)
FMSY <- 0.28

# quota set to fraction of FMSY as per precautionary principle
FMSY_buffer <- 2/3

#percent of population measured for biomass estimation (0.001 = 0.1%)
sampling_pop <- 0.01
# number of years to use in biomass estimate
biomass_est_n_years <- 5

#minimum size caught by nets (cm) from Feekings et al. 2013
min_size <- 38

# target protection level in proportion (e.g. 0.2 is 20% protection)
MPA_coverage <- 0.10
#
# # coastal:marine ratio for MPAs (e.g. CtoM <- 0.4 is 60% marine and 40% coastal in terms of area)
# CtoM <- 0.0009422693

#### load existing MPAS ####
require(rgdal)
oldMPA <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="MPAs_mar")
oldMPA <- spTransform(oldMPA,CRS(proj))
#remove those smaller than cell size
oldMPA <- oldMPA[gArea(oldMPA,byid = TRUE)>=cell_size^2,]

# remove size distribution below grid size
# MPA_size_dist_mar <- MPA_size_dist_mar[MPA_size_dist_mar$breaks>=log10(cell_size^2),]
# MPA_size_dist_mar$prob <- MPA_size_dist_mar$counts/sum(MPA_size_dist_mar$counts)
# round(MPA_size_dist_mar$breaks^10/cell_size^2)

# fixed distance for setting MPA distance in km in fixed distance scenario
fixdist <- 75

# protection scenarios to include in analysis current options include "Status_quo","MPAs_maxdist","MPAs_fixed","MPAs_targeted"
# see protection_scenarios.R and functions.R for more information on protection scenarios
# you must include "Status_quo" or cost evaluation will not work
# protect_scen <- c("Status_quo","MPAs_maxdist","MPAs_fixed","MPAs_targeted")
# protect_scen_names <- c("Status Quo", "Maximum Distance", "Fixed Distance" , "Targeted")
# protect_scen_colour <- c("purple","green","red","blue")
protect_scen <- c("Status_quo","MPAs_targeted")
protect_scen_names <- c("Status Quo","Targeted")
protect_scen_colour <- c("red","blue")


# country name for coastline download for new "coastal" MPA placement (from package maptools in data(wrld_simpl))
country_name  <- "Canada"

#### load pre-existing MPAs (shapefiles generated by MPA_size.R) ####
require(rgdal)
EEZ <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="eez_iho_union_v2")

# remove Canadian part of the Davis Strait for EEZ
EEZ <- EEZ[EEZ$marregion!="Canadian part of the Davis Strait",]


#### load cod habitat and breeding sites ####
# shapefile generated by georeferencing figure 2 from Lough 2004
Habitats <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="cod_habitat")
Breeding <- readOGR(dsn=paste0(getwd(),"/shapefiles"),layer="cod_breeding")[2:15,] # remove first Breeding zone, outside EEZ

# minimum_fishable_biomass is to prevent errors occuring when fisherman are trying to fish with no fish
minimum_fishable_biomass <- 10000

########################################### cost evaluation ##############################################
# normal operating cost ($) per fisherman from (Department of Fisheries and Oceans, 2007, Table A.19 Mixed Fishery Fleet)
# labour (CAD $46587) and fuel ($9008) vary with distance, the remainder does not
fish_operating_cost_ratio <- (46587+9008)/105054 # (labour+fuel)/total
# default profitability to calibrate operating cost (Department of Fisheries and Oceans, 2007, Table A.19 and 5.7 Mixed Fishery Fleet)
Status_quo_profitability <- 166184/105054 #$ catch value/$ operating expense

# landed value for cod CAD/t (http://www.dfo-mpo.gc.ca/stats/commercial/sea-maritimes-eng.htm)
fish_landed_value <- 1.24*1000

# Cost of maintaining an MPA 2698 USD/km2/year (Balmford 2004), 1.21 is current exchange rate to CAD
# MPA_maintenance_cost <- 2698*1.21

# Social discount rates (choose 3)
SDR <- c(0.015,0.03,0.06)
