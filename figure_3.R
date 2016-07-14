
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
         plantplant = as.factor(plantplant))

m5 <- glm(spp ~ poly(scale_averagewater, degree=2) + scale_short + plantplant + year + round -1, allvegmaster, family=binomial())


binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"),color="black", ...)
}

short_log <- ggplot(allvegmaster, aes(x=short, y=spp)) + geom_point() +
  binomial_smooth()+theme_krementz()+xlab("Percent Cover of Annual Moist Soil Vegetation") + ylab("Probability of Sora Presence")

allvegmast <- allvegmaster %>% 
  mutate(round = paste0("Visit ",round))

water_log <- ggplot(allvegmast, aes(x=averagewater, y=spp)) + geom_point() +
  binomial_smooth()+ facet_wrap(~round, nrow=1)+theme_krementz()+xlab("Average Water Depth (cm)") + ylab("Probability of Sora Presence")

new_dat <- data.frame(plantplant = unique(allvegmaster$plantplant),
                      scale_averagewater=median(allvegmaster$scale_averagewater),
                      scale_short = median(allvegmaster$scale_short, na.rm=TRUE),
                      year = 2013,
                      round = unique(allvegmaster$round)[1])

plantpred <- as.data.frame(stats::predict(m5, newdat=new_dat, type="response",interval="predict", level=0.95, se.fit=TRUE))

plantpred$upper <- plantpred$fit + (plantpred$se.fit * 1.96)

plantpred$lower <- plantpred$fit - (plantpred$se.fit * 1.96)

plantpred$cat <- new_dat$plantplant

plant_plot <- ggplot(data=plantpred, aes(x=cat, y=fit, ymin=lower, ymax=upper)) + geom_point() + geom_errorbar() + theme_krementz() + ylim(0,1) +xlab("Genus") + theme(axis.title.y=element_blank())

png(file="~/Dissertation_Chapter_2_Sora_Habitat/figure_3_log_regression.png", height=10, width=13, units="in", res=600)
ggdraw()+
  draw_plot(water_log, 0, 0.5, 1, 0.5)+
  draw_plot(short_log, 0,0,0.5,0.5) +
  draw_plot(plant_plot, 0.5, 0, 0.5, 0.5)
dev.off()

