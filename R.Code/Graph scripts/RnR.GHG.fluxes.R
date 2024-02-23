#'                                     [RnR]
#' Running a two-way anova for the RnR ghg fluxes
#' fixed factors are plot conditions and site (1 2 3)
#' 
#' see if there is an interaction for plot condition and site  
#' 
#packages
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(agricolae)
library(plyr)

#' [Run the two way anova for CH4 flux]
#' Run a to anova for each type of chamber 
#' fix the data frame  
#' 
#' 
#' 
#red in the files, view, rename, clean ect...
GHG_flux_raw <- read.csv("GHG_flux_raw_2023-07.csv") #reading in 
GHG_flux_raw_2023_10 <- read.csv("GHG_flux_raw_2023_10_2.csv")

View(GHG_flux_raw)
View(GHG_flux_raw_2023_10)

str(GHG_flux_raw)
str(GHG_flux_raw_2023_10)
#change the data type at mulitple columns at once
GHG_flux_raw_2023_10 <- mutate_at(GHG_flux_raw_2023_10, vars(ch4_sd,co2_sd), as.double)

GHG_flux_raw <- GHG_flux_raw[-53:-104,]#clean empty rows
GHG_flux_raw_2023_10 <- GHG_flux_raw_2023_10[-53:-104,-21]#clean empty rows

#append the two data sets I want together
all_RnR <- bind_rows(GHG_flux_raw, GHG_flux_raw_2023_10) #append the July and Oct together
View(all_RnR)


#Run the factorial ANOVA aov(data$number~data$factor*data$factor)
CH4.clear <- subset(GHGflux, chamber_type=="clear")
CH4.dark <- subset(GHGflux, chamber_type=="dark")
CH4.soil <- subset(GHGflux, chamber_type=="soil")
CH4.clear.all <- subset(all_RnR, chamber_type=="clear")
CH4.dark.all <- subset(all_RnR, chamber_type=="dark")
CH4.soil.all <- subset(all_RnR, chamber_type=="soil")
CH4.solcle.all <- subset(all_RnR, chamber_type!="dark")
View(CH4.solcle.all)


#View dist
view <- ggplot(CH4.clear.all, aes(x=date, y=ch4_flux, color=site)) + facet_wrap(~plot_type)+
  geom_point() + theme_tufte(base_size=20) + 
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("date", labels=c("July", "Oct.")) + scale_y_continuous("CH4 flux") 
view


#CH4.clear$Trt <- c("S1 S","S1 S","S1 S","S1 U","S1 U","S1 U","S2 S","S2 S","S2 S",
 "S2 U","S2 U","S2 U")
#CH4.soil$Trt <- c("S1 S","S1 S","S1 S","S1 U","S1 U","S1 U","S2 S","S2 S","S2 S",
                   "S2 U","S2 U","S2 U")

#make sure they are subsetted correctly
View(CH4.clear)
View(CH4.dark)
View(CH4.soil)
View(CH4.clear.all)
View(CH4.dark.all)
View(CH4.soil.all)
View(CH4.solcle.all)

#clear aov
CH4.clear.out <- aov(CH4.clear$ch4_flux~CH4.clear$plot_type*CH4.clear$site)
summary(CH4.clear.out)

FANOVA.out2 <- aov(ch4_flux~Trt, CH4.clear)
summary(FANOVA.out2)

CRD <- HSD.test(FANOVA.out2, "Trt")
CRD
yee <- ddply(CH4.soil, .(plot_type, site), summarize, meanSL= mean(ch4_flux, na.rm=T))
yee

CH4flux10.clear.out <- aov(CH4flux_10$ch4_flux~CH4flux_10$plot_type*CH4flux_10$site)
summary(CH4flux10.clear.out)

#clear.July.Oct glm??

#dark aov 
#CH4.dark[8,11] <- NA #this was an outlier talk to sam and Adam 
CH4.dark.out <- aov(CH4.dark$ch4_flux~CH4.dark$plot_type*CH4.dark$site)
summary(CH4.dark.out)

FANOVA.out2 <- aov(ch4_flux~Trt, CH4.dark)
summary(FANOVA.out2)

CRD <- HSD.test(FANOVA.out2, "Trt")
CRD
View(CH4.soil)

#soil aov
CH4.soil.out <- aov(CH4.soil$ch4_flux~CH4.soil$plot_type*CH4.soil$site)
summary(CH4.soil.out)

library(plyr)
x <- ddply(CH4.soil,.(site), summary, mean=mean(ch4_flux))
print(x)

FANOVA.out2 <- aov(ch4_flux~Trt, CH4.soil)
summary(FANOVA.out2)

CRD <- HSD.test(FANOVA.out2, "Trt")
CRD

#test for assumptions
Fres <- residuals(CH4.soil.out)
hist(Fres)
shapiro.test(Fres)

#dont run a post hoc because youll have nine different factors to compare, too much

#'[Graph them]


#clear

#old July graph 
CH4.clear.graph <- ggplot(CH4.clear, aes(x=site, y=ch4_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20) + 
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CH4 flux (ppb)") +
scale_fill_manual(values=c("salmon2", "turquoise3"), 
                  name="Plot condition", labels=c("Stable","Unstable"))
CH4.clear.graph

#July+Oct clear
CH4.clear.all <- mutate_at(CH4.clear.all, vars(date), as.character) #date must be read as Chr 

# make a mean for each group I am interested in to make a point plot. 
ddply.CH4.clear.all <- ddply(CH4.clear.all, .(site,date,plot_type), summarize, Mean= mean(ch4_flux, na.rm=T),
                     sd=sd(ch4_flux, na.rm=T), se= sd(ch4_flux,na.rm=T)/sqrt(length(ch4_flux)))

View(ddply.CH4.clear.all)

#apply levels to the date to try and group it (not sure if this is needed step)
ddply.CH4.clear.all$date = factor(ddply.CH4.clear.all$date, levels=c("2023-07-11", 
                                                               "2023-10-10")
#plot GHG for clear RnR July + Oct
tryout <- ggplot(ddply.CH4.clear.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
  geom_point(position = position_dodge(width=0.4), size = 3) +  
  geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                width = 0.25)+ #fixed bars here by adding width on outside
  theme_tufte(base_size=20)+
  coord_cartesian(ylim = c(-100, 100))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
  scale_y_continuous("", breaks=seq(-100,100, by=25))+
  scale_x_discrete("Site", labels= c("1", "2","2", "4"))+
  scale_shape_manual(values = c(16, 17))

  
tryout


#July + Oct clear-soil flux, (ecosytem level)
View(CH4.solcle.all)

resultz <- ddply(CH4.solcle.all, .(plot_ID,date,site,plot_type), 
                summarize, 
                Difference = (ch4_flux[chamber_type == "clear"] - ch4_flux[chamber_type == "soil"]))
              
                         
View(resultz)

result2 <- mutate_at(result2, vars(date), as.character) #date must be read as Chr 


# make a mean for each group I am interested in to make a point plot. 
result2 <- ddply(resultz, .(site,date,plot_type), summarize, Mean= mean(Difference, na.rm=T),
                             sd=sd(Difference, na.rm=T), se= sd(Difference,na.rm=T)/sqrt(length(Difference)))

View(result2)

#apply levels to the date to try and group it (not sure if this is needed step)
result2$date = factor(result2$date, levels=c("2023-07-11", 
                                                                     "2023-10-10")
                #plot GHG for clear RnR July + Oct
Just.plant<- ggplot(result2, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
  geom_point(position = position_dodge(width=0.4), size = 3) +  
  geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                width = 0.25)+ #fixed bars here by adding width on outside
  theme_tufte(base_size=20)+
  coord_cartesian(ylim = c(-100, 100))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
  scale_y_continuous("", breaks=seq(-100,100, by=25))+
  scale_x_discrete("Site", labels= c("1", "2","2", "4"))+
  scale_shape_manual(values = c(16, 17))
                                  
                                  
Just.plant
                                  



#dark
CH4.dark.graph <- ggplot(CH4.dark, aes(x=site, y=ch4_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20) +
  theme(axis.line= element_line(colour="black", size=.5), 
          axis.ticks.length=unit(-0.25, "cm"), 
          axis.ticks.x=element_blank(), 
          axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
          axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CH4 flux (ppb)") +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))
CH4.dark.graph

#July+Oct dark
# make a mean for each group I am interested in to make a point plot. 
ddply.CH4.dark.all <- ddply(CH4.dark.all, .(site,date,plot_type), summarize, Mean= mean(ch4_flux, na.rm=T),
                             sd=sd(ch4_flux, na.rm=T), se= sd(ch4_flux,na.rm=T)/sqrt(length(ch4_flux)))

View(ddply.CH4.dark.all)
str(ddply.CH4.dark.all)
ddply.CH4.dark.all$date <- as.character(ddply.CH4.dark.all$date)

#apply levels to the date to try and group it (not sure if this is needed step)
ddply.CH4.clear.all$date = factor(ddply.CH4.clear.all$date, levels=c("2023-07-11", "2023-10-10"))
 #plot GHG for clear RnR July + Oct
  tryout2 <- ggplot(ddply.CH4.dark.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
                    geom_point(position = position_dodge(width=0.4), size = 3) +  
                    geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                                  width = 0.25)+ #fixed bars here by adding width on outside
                    theme_tufte(base_size=20)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
                    theme(axis.line= element_line(colour="black", size=.5), 
                          axis.ticks.length=unit(-0.25, "cm"), 
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
                          axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
                    scale_y_continuous("CH4 flux (μmolCH4 per hour)")+
                    scale_x_discrete("Site", labels= c("1", "2", "2", "4"))+
    scale_shape_manual(values = c(16, 17))
                                  
                                  
tryout2


#soil 
CH4.soil.graph <- ggplot(CH4.soil, aes(x=site, y=ch4_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20)+ 
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CH4 flux (ppb)") +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))
CH4.soil.graph

#July+Oct soil

ddply.CH4.soil.all <- ddply(CH4.soil.all, .(site,date,plot_type), summarize, Mean= mean(ch4_flux, na.rm=T),
                            sd=sd(ch4_flux, na.rm=T), se= sd(ch4_flux,na.rm=T)/sqrt(length(ch4_flux)))

View(ddply.CH4.soil.all)
str(ddply.CH4.soil.all)
ddply.CH4.soil.all$date <- as.character(ddply.CH4.soil.all$date)
ddply.CH4.soil.all$site <- as.factor(ddply.CH4.soil.all$site)


#apply levels to the date to try and group it (not sure if this is needed step)
#ddply.CH4.clear.all$date = factor(ddply.CH4.clear.all$date, levels=c("2023-07-11", 
#                                                                    "2023-10-10")
#plot GHG for clear RnR July + Oct
tryout3 <- ggplot(ddply.CH4.soil.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
  geom_point(position = position_dodge(width=0.4), size = 3) +  
  geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                width = 0.25)+ #fixed bars here by adding width on outside
  theme_tufte(base_size=20)+
  coord_cartesian(ylim = c(-100, 100))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
  scale_y_continuous("CH4 flux (μmolCH4 per hour)", breaks=seq(-100,100, by=25))+
  scale_x_discrete("Site", labels= c("1", "2", "2", "4"))+
  scale_shape_manual(values = c(16, 17))

library(patchwork)

tryout3



 result <- tryout3+Just.plant+tryout+plot_layout(guides = "collect")+ 
            plot_annotation(theme(axis.title.y = element_blank()))&
            theme(legend.position = "bottom",axis.title.y = element_blank())
result


#'[Run the two way anova for C02 flux]
#
#run two way anova for CO2 
#Clear
View(CH4.clear)
CO2.clear.out <- aov(CH4.clear$co2_flux~CH4.clear$plot_type*CH4.clear$site)
summary(CO2.clear.out)

FANOVA.out2 <- aov(co2_flux~site, CH4.clear)
summary(FANOVA.out2)

CRD <- HSD.test(FANOVA.out2, "site")
CRD




#dark aov 
CO2.dark.out <- aov(CH4.dark$co2_flux~CH4.dark$plot_type*CH4.dark$site)
summary(CO2.dark.out)
#soil aov
CO2.soil.out <- aov(CH4.soil$co2_flux~CH4.soil$plot_type*CH4.soil$site)
summary(CO2.soil.out)

FANOVA.out2 <- aov(co2_flux~Trt, CH4.soil)
summary(FANOVA.out2)

CRD <- HSD.test(FANOVA.out2, "plot_type")
CRD


#test for assumptions
Fres <- residuals(CO2.clear.out)
qqnorm(Fres)
hist(Fres)
shapiro.test(Fres)

#graph CO2
#clear
CO2.clear.graph <- ggplot(CH4.clear, aes(x=site, y=co2_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20) + 
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CO2 flux (ppm)") +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))
  
CO2.clear.graph


#CO2 July+Oct soil

ddply.Co2.clear.all <- ddply(CH4.clear.all, .(site,date,plot_type), summarize, Mean= mean(co2_flux, na.rm=T),
                            sd=sd(co2_flux, na.rm=T), se= sd(co2_flux,na.rm=T)/sqrt(length(co2_flux)))

View(ddply.Co2.clear.all)
str(ddply.Co2.clear.all)
ddply.Co2.clear.all$date <- as.character(ddply.Co2.clear.all$date)


#apply levels to the date to try and group it (not sure if this is needed step)
ddply.CH4.clear.all$date = factor(ddply.CH4.clear.all$date, levels=c("2023-07-11", 
                                                                    "2023-10-10")
#plot GHG for clear RnR July + Oct
Co2.clearJulyOct <- ggplot(ddply.Co2.clear.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
  geom_point(position = position_dodge(width=0.4), size = 3) +  
  geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                width = 0.25)+ #fixed bars here by adding width on outside
  theme_tufte(base_size=20)+
  coord_cartesian(ylim = c(-100, 25))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
  scale_y_continuous("CO2 flux (mmol CO2 per hour)",breaks=seq(-100,20, by=20))+
  scale_x_discrete("Site", labels= c("1", "2", "2", "4"))+
  scale_shape_manual(values = c(16, 17))


Co2.clearJulyOct


#CO2.   July + Oct clear-soil flux, (ecosytem level)


CH4.solcle.all <- mutate_at(CH4.solcle.all, vars(date), as.character) #date must be read as Chr 

resultzco2 <- ddply(CH4.solcle.all, .(plot_ID,date,site,plot_type), #get the difference
                 summarize, 
                 Difference = (co2_flux[chamber_type == "clear"] - co2_flux[chamber_type == "soil"]))

#average the differences
coolresult2 <- ddply(resultzco2, .(site,date,plot_type), summarize, Mean= mean(Difference, na.rm=T),
                     sd=sd(Difference, na.rm=T), se= sd(Difference,na.rm=T)/sqrt(length(Difference)))

 #plot GHG for just plants Co2 RnR July + Oct
     co2.Just.plant<- ggplot(coolresult2, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
       geom_point(position = position_dodge(width=0.4), size = 3) +  
       geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                     width = 0.25)+ #fixed bars here by adding width on outside
       theme_tufte(base_size=20)+
       coord_cartesian(ylim = c(-100, 25))+
       geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
       theme(axis.line= element_line(colour="black", size=.5),
             axis.ticks.length=unit(-0.25, "cm"), 
             axis.ticks.x=element_blank(), 
             axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
             axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
       scale_y_continuous("", breaks=seq(-100,20, by=20))+
       scale_x_discrete("Site", labels= c("1", "2","2", "4"))+
       scale_shape_manual(values = c(16, 17))
                      
                      
   co2.Just.plant




#dark
CO2.dark.graph <- ggplot(CH4.dark, aes(x=site, y=co2_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20)+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CO2 flux (ppm)") +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))

CO2.dark.graph

#Co2 dark July + Oct 
ddply.Co2.dark.all <- ddply(CH4.dark.all, .(site,date,plot_type), summarize, Mean= mean(co2_flux, na.rm=T),
                             sd=sd(co2_flux, na.rm=T), se= sd(co2_flux,na.rm=T)/sqrt(length(co2_flux)))

View(ddply.Co2.dark.all)
str(ddply.Co2.dark.all)
ddply.Co2.dark.all$date <- as.character(ddply.Co2.dark.all$date)


#apply levels to the date to try and group it (not sure if this is needed step)
ddply.CH4.dark.all$date = factor(ddply.CH4.dark.all$date, levels=c("2023-07-11", 
                                                                     "2023-10-10")
 #plot GHG for clear RnR July + Oct
 Co2.darkJulyOct <- ggplot(ddply.Co2.dark.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
   geom_point(position = position_dodge(width=0.4), size = 3) +  
   geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                 width = 0.25)+ #fixed bars here by adding width on outside
   theme_tufte(base_size=20)+
 geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
 theme(axis.line= element_line(colour="black", size=.5), 
       axis.ticks.length=unit(-0.25, "cm"), 
       axis.ticks.x=element_blank(), 
       axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
       axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
   scale_y_continuous("CO2 flux (mmol CO2 per hour)")+
   scale_x_discrete("Site", labels= c("1", "2", "2", "4"))+
   scale_shape_manual(values = c(16, 17))
 
 Co2.darkJulyOct

#soil 
CO2.soil.graph <- ggplot(CH4.soil, aes(x=site, y=co2_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20)+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CO2 flux (ppm)", breaks = seq(-5,30, by=5)) +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))
CO2.soil.graph

#Co2 soil July+Oct
ddply.Co2.soil.all <- ddply(CH4.soil.all, .(site,date,plot_type), summarize, Mean= mean(co2_flux, na.rm=T),
                            sd=sd(co2_flux, na.rm=T), se= sd(co2_flux,na.rm=T)/sqrt(length(co2_flux)))

View(ddply.Co2.soil.all)
str(ddply.Co2.soil.all)
ddply.Co2.soil.all$date <- as.character(ddply.Co2.soil.all$date)


#apply levels to the date to try and group it (not sure if this is needed step)
ddply.CH4.soil.all$date = factor(ddply.CH4.soil.all$date, levels=c("2023-07-11", 
                                                                   "2023-10-10"))
#plot GHG for clear RnR July + Oct
Co2.soilJulyOct <- ggplot(ddply.Co2.soil.all, aes(x=interaction(site,date), y=Mean, color = plot_type, shape = date)) + 
  geom_point(position = position_dodge(width=0.4), size = 3) +  
  geom_errorbar(aes(ymax=Mean+ sd, ymin=Mean-sd),position = position_dodge(width = .4), 
                width = 0.25)+ #fixed bars here by adding width on outside
  theme_tufte(base_size=20)+
  coord_cartesian(ylim = c(-100, 25))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm")))+
  scale_y_continuous("CO2 flux (mmol CO2 per hour)",breaks=seq(-100,20, by=20))+
  scale_x_discrete("Site", labels= c("1", "2", "2", "4"))
                                 
Co2.soilJulyOct
       


co2results <- Co2.soilJulyOct+co2.Just.plant+Co2.clearJulyOct+plot_layout(guides = "collect")+ 
  plot_annotation(theme(axis.title.y = element_blank()))&
  theme(legend.position = "bottom",axis.title.y = element_blank())
co2results



                          


#'[regression of GHG and plant biomass

#calculated avg biomass for each plot and added it to the desired CH4 subset 

View(CH4.clear)
CH4.clear$avgBiomass <- c(2.846004548,
                          3.414631669,
                          1.573720355,
                          1.508796351,
                          3.030962595,
                          2.665892597,
                          5.384327766,
                          4.929319849,
                          3.267410693,
                          1.470740455,
                          5.540695444,
                          0.860260417)


RegCH4Plant.out <- lm(ch4_flux~avgBiomass,CH4.clear) #lm(dependent(y)~independent(x),data)
summary(RegCH4Plant.out)
anova(RegCH4Plant.out)#analysis of F-ratio 




#Test assumptions
model1 <- lm(ch4_flux~avgBiomass,CH4.clear)
summary(model1)
res <- residuals(model1)
shapiro.test(res)

#visual validation of normal distribution 
qqnorm(res)
qqline(res)

View

model <- lm(ch4_flux~avgBiomass,CH4.clear)
summary(model)

a<-ggplot(CH4.clear, aes(x = avgBiomass, y = ch4_flux,)) + geom_point(size=3)
a

b<-a + geom_smooth(method="lm",   
                   se=FALSE) +theme_tufte(base_size=15) +geom_rangeframe(color="black")+
  scale_x_continuous("Average Biomass (g)", labels=c("1","","2","","3","","4","","5","","6"),breaks=seq(1,6, by=.5)) + 
  scale_y_continuous("", labels=c("-50","","0","","50","","100","","150"), breaks=seq(-50,150, by=25) )
  
b



hist(CH4.clear.all$ch4_flux)
View(CH4.clear.all)
ggplot(CH4.clear.all, aes(x = ch4_flux, fill = plot_type)) +
  geom_histogram(position = "identity", alpha = 0.5)

ggplot(CH4.soil, aes(x=site, y=ch4_flux, fill=plot_type)) + 
  geom_boxplot() + theme_tufte(base_size=20)+
  theme(axis.line= element_line(colour="black", size=.5), 
        axis.ticks.length=unit(-0.25, "cm"), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_text(margin=unit(c(.5,.5,.5,.5),"cm")), 
        axis.text.y=element_text(margin=unit(c(.5,.5,.5,.5),"cm"))) +
  scale_x_discrete("Site", labels= c("1", "2")) + scale_y_continuous("CO2 flux (ppm)", breaks = seq(-5,30, by=5)) +
  scale_fill_manual(values=c("salmon2", "turquoise3"), 
                    name="Plot condition", labels=c("Stable","Unstable"))




