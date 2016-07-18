# Needed Packages

library(unmarked)
library(raildata)
# Loading the data

load("~/raildata/data/soraDONE.rda") # or soraDONE.csv
load("~/raildata/data/vegDONE.rda")

# make sure that the two files are lined up

sora <- soraDONE[order(soraDONE$iry),]
cov <- vegDONE[order(vegDONE$iry),]


# these are the distance bins, in meters
cutpt = as.numeric(c(0,1,2,3,4,5)) 

# create the water squared variable

cov$scale_averagewater2 <- cov$scale_averagewater^2

# removes instances where length is not recorded

cov <- cov[!is.na(cov$length),]

# makes year categorical

cov$year <- as.factor(cov$year)

# makes it so that the sora and covariate file have only the same entries

sora <- sora[sora$iry %in% cov$iry,]

# brings the two files together into the ummarkedFrameGDS
umf = unmarkedFrameGDS(y=sora[,1:5], 
                       numPrimary=1,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length
)

# creates the list to store the models in
density.modelsP<-list()


# interspersion model

# density.modelsP$int <- gdistsamp(~scale_int + year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

# water model

density.modelsP$water <- gdistsamp(~scale_averagewater + year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

# water squared model

density.modelsP$water2 <- gdistsamp(~scale_averagewater+scale_averagewater2 + year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

# global model

density.modelsP$global <- gdistsamp(~scale_averagewater+scale_averagewater2+scale_short+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

# null model

density.modelsP$null <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

# non persistent moist soil (aka short) vegetation percent cover + interspersion

density.modelsP$short <- gdistsamp(~scale_short+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

## Assemble the various model fits into a "fitList" and do model selection
fits.density <- fitList(fits=density.modelsP)
(ms.density <- modSel(fits.density))


save(density.modelsP, file="~/manuscripts/Dissertation_Chapter_2_Habitat_Sora/poisson_models.Rdata")


