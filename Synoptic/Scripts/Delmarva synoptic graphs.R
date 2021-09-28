# Preliminary data from DMV wetlands

# Really just graphs for the prospectus, work on more later

library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(methods)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Data")
DMV_nov20 <- read.csv("202011_GHG_Wetlands_new_columns.csv")
attach(DMV_nov20)

   
########## Subsetting the data ###########

# # How to subset
# work with a subset of the air background samples
# air <- data_202011[ which(data_202011$Rep=="Air"),]

# Core synoptic sites
core_sites <- DMV_nov20[ which(DMV_nov20$Site_ID == "DB-SW" | Site_ID == "TB-SW"
                         | Site_ID == "FN-SW" | Site_ID == "QB-SW" | Site_ID == "DF-SW"
                         | Site_ID == "TI-SW" | Site_ID == "JA-SW" | Site_ID == "JC-SW"
                         | Site_ID == "JB-SW" | Site_ID == "NB-SW"),]

core_forested <- DMV_nov20[ which(DMV_nov20$Site_ID == "DB-SW" | Site_ID == "TA-SW"
                               | Site_ID == "TB-SW" | Site_ID == "QB-SW"
                               | Site_ID == "TI-SW" | Site_ID == "JA-SW" | Site_ID == "JB-SW"
                               | Site_ID == "JC-SW" | Site_ID == "NB-SW" | Site_ID == "DK-SW" 
                               | Site_ID == "TS-SW" | Site_ID == "BD-SW" | Site_ID == "ND-SW"),]

core_forested$Site_ID <- factor(core_forested$Site_ID, levels=c("DB-SW","TA-SW","TB-SW",
                                                                "QB-SW","TI-SW","JA-SW",
                                                                "JB-SW","JC-SW", "NB-SW",
                                                                "DK-SW", "TS-SW", "BD-SW", "ND-SW"), order=TRUE)

sensor <- DMV_nov20[ which(DMV_nov20$Site_ID == "DK-SW" | Site_ID == "TS-SW"
                                            | Site_ID == "BD-SW" | Site_ID == "ND-SW"),]
# All sites (surface water)
SW_sites <- DMV_nov20[ which(DMV_nov20$Sample_type == "SW"),]


######## Graph some data ###########

### [1] CH4 concentrations across sites

# Core sites
ggplot(core_sites, aes(x = Site, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"))
ggsave("CH4_202011_new.jpg")

# All synoptic sites + rivers

ggplot(SW_sites, aes(x = Site, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme(axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
ggsave("CH4_202011_new.jpg")

### [2] CO2 concentrations across sites

# Core sites

ggplot(core_forested, aes(x = Site, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))
ggsave("CO2_202011_forested.jpg")

ggplot(sensor, aes(x = Site, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"))
ggsave("CO2_202011_sensor.jpg")


#  annotate("rect", ymin = 3965, ymax = 3970, xmin = as.POSIXct("DB"), xmax = as.POSIXct("TI"), alpha = 0.9) +
#  annotate("text", x= as.POSIXct("JA"), y = 3970, label = "María", angle = 0,  color="red", size =4) +

# annotate("rect", xmin = as.POSIXct("2017-09-19"), xmax = as.POSIXct("2017-09-22"), 
           ymin = 0, ymax = 500, alpha = 0.9) +
#  annotate("text", x = as.POSIXct("2017-09-16"), y = 400, label = "María", angle = 90, 
           color="red", size =4) +

# All synoptic sites + rivers
ggplot(SW_sites, aes(x = Site, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme(axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
ggsave("CO2_202011_new.jpg")


### [3] CH4 against CO2

ggplot(core_sites,aes(x= wCO2_uatm_medhs, y= wCH4_uatm_medhs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0,41000) +
  labs(x = "CO2 concentration (uatm)", y = "CH4 concentrations (uatm)") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))
ggsave("CO2 vs CH4_DMV.jpg") 

summary(lm(formula = wCH4_uatm_medhs~wCO2_uatm_medhs, data = core_sites))


#######################################

ggplot(ghg, aes(x= Sample.Name, y= CO2_uM, color = Sample.Name)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  labs(x = "Site", y= expression(paste("C","O"[2]^{}*" ("*mu,"M)"))) +
  ylim(0, 400) +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

ggsave("Boxplot_CO2.jpg")

# CH4 boxplot for all sites

ggplot(ghg, aes(x= Sample.Name, y= CH4_uM, color = Sample.Name)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_y_log10() +
  labs(x = "Site", y= expression(paste("C","H"[4]^{}*" ("*mu,"M)"))) +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))