library(ResourceSelection)
library(raildata)
library(dplyr)
library(tidyr)
library(gridExtra)
library(auriel)
library(ggplot2)
library(cowplot)
data("allveg") # or allveg.csv

greater05 <- c("smartweed","bulrush","cocklebur","grass","millet","spikerush")
####

gather <- allveg %>%
  filter(bv != "day",
         averagewater<=900,
         year!=2012,
         spp=="sora"|spp=="veg") %>%
  select(round, spp, averagewater, int, short, simple_plant1, simple_plant2, simple_plant3, year) %>%
  gather("variable","value", -spp, -round, -short,-int,-averagewater, - year, na.rm=TRUE) %>%
  filter(value %in% greater05)%>%
  mutate(num =1,
         uni = 1:n())

spread <- gather %>%
            select(uni, num, value) %>%
            spread("value","num") %>%
            select(-uni)

spread[is.na(spread)] <- 0

three <- gather %>%
          select(variable, value, uni) %>%
          spread("variable","value") %>%
          mutate(plantplant = ifelse(!is.na(simple_plant1), simple_plant1, ifelse(!is.na(simple_plant2), simple_plant2, simple_plant3)))  %>%
          select(-uni)

allveg <- cbind(gather, spread, three)

####


allvegmaster <- allveg %>%
  mutate(spp=replace(spp, spp=="sora",1),
         spp=replace(spp, spp=="veg",0),
         spp = as.numeric(spp),
         scale_averagewater = as.numeric(base::scale(averagewater)),
         scale_short = as.numeric(base::scale(short)),
         plantplant = as.factor(plantplant),
         year = as.factor(year),
         round = as.factor(round)) %>%
  filter(!is.na(scale_short))


m1 <- glm(spp ~ scale_averagewater + year + round, allvegmaster, family=binomial())
m2 <- glm(spp ~ scale_averagewater, allvegmaster, family=binomial())
m3 <- glm(spp ~ poly(scale_averagewater, degree=2), allvegmaster, family=binomial())
m4 <- glm(spp ~ poly(scale_averagewater, degree=2) + year + round, allvegmaster, family=binomial())

m5 <- glm(spp ~ poly(scale_averagewater, degree=2) + scale_short + plantplant + year + round, allvegmaster, family=binomial())

m6 <- glm(spp ~ scale_short , allvegmaster, family=binomial())
m7 <- glm(spp ~ scale_short + year + round, allvegmaster, family=binomial())
aic <- AIC(m1,m2,m3,m4,m5, m6, m7)