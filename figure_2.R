# needed packages
library(ggplot2) # needs 2.0.0 for axis.line
library(unmarked)
library(AICcmodavg)
library(gridExtra)
library(grid)
library(dplyr)
library(raildata)
data(vegDONE) # also see vegDONE.csv

load("~/Dissertation_Chapter_2_Sora_Habitat/poisson_models.Rdata") # the models from gdistsamp_mdoel_run.R

cov <- vegDONE

# creating a data frame where water depth varies, but other variables are held cosntant
new_dat <- data.frame(scale_averagewater=c(seq(min(cov$scale_averagewater), max(cov$scale_averagewater), length.out=50)), scale_short=mean(cov$scale_short), year=factor(2013, levels=c(2012,2013,2014,2015)), averagewater=c(seq(min(cov$averagewater),max(cov$averagewater),length.out=50))) %>% mutate(scale_averagewater2 = scale_averagewater^2)

# predicting sora density from the global mode (top model) given our new data frame
pred <- predict(density.modelsP$global, newdat=new_dat, type="lambda")

# binding together the new data frame and our predictions
water.pre <- cbind(new_dat, pred)


(w15 <- ggplot(data=water.pre)+ 
  geom_ribbon(aes(x=averagewater, y=Predicted, ymin=lower, ymax=upper), fill="grey")+
  geom_line(aes(x=averagewater, y=Predicted))+
  ylab("Sora Density \n(Sora/Hectare)")+
  xlab("Average Water Depth (cm)\n ")+
  theme(axis.text.x = element_text(size=18,color="black"),
        axis.text.y = element_text(size=18,color="black"),
        axis.title.y=element_text(size=20),
        plot.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        title=element_text(size=20),
        panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black")
        ))


# creating a data frame where non-persistent moist soil vegetation varies, but other variables are held cosntant
new_dat <- data.frame(scale_short=c(seq(min(cov$scale_short), max(cov$scale_short), length.out=50)),  scale_averagewater=mean(cov$scale_averagewater), year=factor(2013, levels=c(2012,2013,2014,2015)), short=c(seq(min(cov$short),max(cov$short),length.out=50))) %>% mutate(scale_averagewater2 = scale_averagewater^2)

# predicting sora density from the global mode (top model) given our new data frame
pred <- predict(density.modelsP$global, newdat=new_dat, type="lambda")

# binding together the new data frame and our predictions
water.pre <- cbind(new_dat, pred)

(s15 <- ggplot(data=water.pre)+
  geom_ribbon(aes(x=short, y=Predicted, ymin=lower, ymax=upper), fill="grey")+
  geom_line(aes(x=short, y=Predicted))+
  ylab("Sora Density \n(Sora/Hectare)")+
  xlab("Annual Moist Soil Vegetation \nPercent Cover")+
  theme(axis.text.x = element_text(size=18,color="black"),
        axis.text.y = element_text(size=18,color="black"),
        axis.title.y=element_text(size=20),
        plot.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        title=element_text(size=20),
        panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black")
  ))


jpeg(file="~/Dissertation_Chapter_2_Sora_Habitat/chapter2_figure2.jpg", height=15, width=30, unit="cm", res=1200)
grid.arrange(w15,s15,ncol=1)
dev.off()
