library(ResourceSelection)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(raildata)
library(auriel)
library(reshape)
library(grid)

data("allveg") # or allveg.csv

allveg <- allveg[allveg$bv!="day",] # removing the veg points where the bird was detected during the day
allveg <- allveg[allveg$averagewater<=900,] # removing veg points where water is under 900. values entered over 900 indicated that the water was too deep to be measured
allveg <- allveg[!is.na(allveg$bv),] # removing points where the status was not recorded

table2a <- allveg[,c("year","averagewater","short")] %>% 
              gather("variable","value",-year) %>% 
              group_by(year, variable) %>% 
              summarise(median=median(value, na.rm=TRUE), 
                        min=min(value, na.rm=TRUE), 
                        max=max(value, na.rm=TRUE))  %>% 
              select(variable, everything()) %>% 
              ungroup() %>% 
              mutate(year = as.character(year))

table2 <- allveg[,c("year","averagewater","short")] %>% 
            gather("variable","value",-year) %>% 
            group_by(variable) %>% 
            summarise(year="all",
                      median=median(value, na.rm=TRUE), 
                      min=min(value, na.rm=TRUE), 
                      max=as.integer(max(value, na.rm=TRUE))) %>% 
            bind_rows(table2a) %>% 
            arrange(variable, year)

write.csv(table2, file="~/Dissertation_Chapter_2_Sora_Habitat/table2.csv", row.names = FALSE)
