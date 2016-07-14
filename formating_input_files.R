########
# This code creates the input files (both veg and bird related) for input into gdistsamp
# This code specifically stacks all of the surveys from one year into one input file
####

###
# Needed Packages ----------------------------------------------------------------------------------------
###
library(unmarked)
library(dplyr)
library(tidyr)
library(raildata)
dist.breaks <- c(0,1,2,3,4,5) 

data(allbirds) # or allbirds.csv
birds <- allbirds
birds <- birds[birds$species=="sora"|birds$species=="s",] # only the sora records
birds <- birds[birds$distance<=5,]  # only the birds less than or equal to five meters from the line
birds <- birds[!is.na(birds$round),] # remove birds for which we have not recorded the round
birds <- birds[!is.na(birds$distance),] # remove birds with no distance recorded

birds$jdate <- as.factor(birds$odate)
birds$impound <- as.character(birds$impound)
birds[birds$impound=="sanctuarysouth",]$impound <- "sanctuary" # this unit was surveyed in two sections because of logistics but is one unit
birds[birds$impound=="sanctuarynorth",]$impound <- "sanctuary" # this unit was surveyed in two sections because of logistics but is one unit
birds[birds$impound=="n mallard",]$impound <- "nmallard" #remaining to remove the space
birds[birds$impound=="r4/5",]$impound <- "r45" # remaining to remove the special character

birds$iry <- paste0(birds$impound,"_",birds$round,"_",birds$year)

########################## ----------------------------------------------------------------------------------------


gd <- as.data.frame(formatDistData(birds, "distance","iry",dist.breaks))
gd$iry <- row.names(gd)


####-----------------------------------------------
# Input covariates ----------------------------------------------------------------------------------------
####----------------------------------------------

data(allveg) # or allveg.csv

veg <- allveg
veg <- veg[veg$averagewater<=100,]
veg <- veg[!is.na(veg$averagewater),]

veg$impound <- as.character(veg$impound)

veg[veg$impound=="n mallard",]$impound <- "nmallard" #remaining to remove the space
veg[veg$impound=="r4/5",]$impound <- "r45" #remaining to remove the special character
veg[veg$impound=="redhead slough",]$impound <- "redhead" #remaining to remove the space

veg$iry <- paste0(veg$impound,"_",veg$round,"_",veg$year)

veg$iy <- paste0(veg$impound,"_",veg$year)


water <- veg[,c( "region","iry","iy", "averagewater")] %>% gather("variable","value",-iry,-iy,-region)
veg <- veg[,c("iy","int","short")] %>% gather("variable","value",-iy)

sumwater <- water %>% group_by(iry, iy, region) %>% summarize(median=median(value, na.rm=TRUE))

sumveg <- veg %>% group_by(iy, variable) %>% summarize(median=median(value, na.rm=TRUE)) %>% spread(variable, median)

all_veg <- merge(sumwater, sumveg, by="iy", all=FALSE)

colnames(all_veg)[4] <- "averagewater"

data(allsurveys) # or allsurveys.csv
surv <- allsurveys

surv$impound <- as.character(surv$impound)

surv[surv$impound=="n mallard",]$impound <- "nmallard" #remaining to remove the space

surv[surv$impound=="r4/5",]$impound <- "r45"  #remaining to remove the special character

surv$iry <- paste0(surv$impound,"_",surv$round,"_",surv$year)

mjdate <- surv[,c("jdate","iry")] %>% gather("variable","value",-iry) %>% group_by(variable, iry) %>% summarize(median=median(value, na.rm=TRUE)) %>% spread(variable, median)

clength <- surv[,c("iry","length")] %>% gather("variable","value",-iry) %>% group_by(variable, iry) %>% summarize(sum=sum(value, na.rm=TRUE)) %>% spread(variable, sum)

clength$length <- clength$length*1000

all_veg_jdate <- merge(all_veg, mjdate, by="iry", all.x=TRUE)
all_veg_jdate_length <- merge(all_veg_jdate, clength, by="iry",all.x=TRUE)

all_veg_jdate_length$scale_short <- scale(all_veg_jdate_length$short)
all_veg_jdate_length$scale_int <- scale(all_veg_jdate_length$int)
all_veg_jdate_length$scale_averagewater <- scale(all_veg_jdate_length$averagewater)

all_veg_jdate_length_year <- all_veg_jdate_length %>% separate(iy, into=c("impound","year"), by=-5)

veg_DONE <- all_veg_jdate_length_year[(all_veg_jdate_length_year$iry %in% gd$iry),]

sora_DONE <- gd[(gd$iry %in% all_veg_jdate_length_year$iry),]

nrow(sora_DONE) == nrow(veg_DONE)
unique(sora_DONE$iry) == unique(veg_DONE$iry)

write.csv(sora_DONE, "./data/sora_DONE.csv", row.names=F)
write.csv(veg_DONE, "./data/veg_DONE.csv", row.names=F)
