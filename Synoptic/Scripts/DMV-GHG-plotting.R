# Script for plotting Delmarva dissolved GHG synoptic samples
# Carla López Lloreda
# Last updated 10/6/2021

# Load libraries

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(methods)

# Set working directory
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in processed GHG data for all synoptic sampling dates
GHG_2105_complete <- read_csv("2021-05/202105_GHG_Wetlands.csv")
GHG_2011_complete <- read_csv("2020-11/202011_GHG_Wetlands.csv")
   
#### Subsetting the data ####

# Subsetting to include only sample info and GHG concentrations & remove NAs
GHG_2105 <- select(GHG_2105_complete, Site, Sample_Type, Site_ID, Sample_Date, wCO2_uatm_medhs, wCH4_uatm_medhs) %>%
  na.omit(GHG_2105)
GHG_2011 <- select(GHG_2011_complete, Site, Sample_Type, Site_ID, Sample_Date, wCO2_uatm_medhs, wCH4_uatm_medhs) %>%
  na.omit(GHG_2011)

# Subsetting by catchment
GHG_2105_ND <- filter(GHG_2105, Site == "ND") # Only North Dogbone SW and GW sites
GHG_2011_ND <- filter(GHG_2011, Site == "ND") # Only North Dogbone SW and GW sites

# Trying to filter for all Jackson Lane sites # figuring this out will be useful down the line
GHG_2105_JL <- filter(GHG_2105, Sample_Type =="SW" & Site == "ND")

# Rearrange factor order from upstream to downstream
GHG_2011_ND$Site_ID <- factor(GHG_2011_ND$Site_ID,levels = c("ND-UW1", "ND-SW", "ND-UW2"))
GHG_2105_ND$Site_ID <- factor(GHG_2105_ND$Site_ID, levels = c("ND-UW1", "ND-SW", "ND-UW2"))



# Subsetting by sample type
GHG_2105_SW <- filter(GHG_2105, Sample_Type == "SW")  # Surface water
GHG_2105_GW <- filter(GHG_2105, Sample_Type == "UW")  # Groundwater
GHG_2105_CH <- filter(GHG_2105, Sample_Type == "CH")  # Channels

GHG_2011_SW <- filter(GHG_2011, Sample_Type == "SW")  # Surface water
GHG_2011_GW <- filter(GHG_2011, Sample_Type == "UW")  # Groundwater
GHG_2011_CH <- filter(GHG_2011, Sample_Type == "CH")  # Channels


##### Stat summaries ####

# Getting average by site
CO2_means <- GHG_2011 %>%
  group_by(Site_ID) %>%
  summarize(mean_CO2 = mean(wCO2_uatm_medhs, na.rm = TRUE))
write.csv(CO2_means, "CO2_means_PCA.csv")

CH4_means <- GHG_2011 %>%
  group_by(Site_ID) %>%
  summarize(mean_CH4 = mean(wCH4_uatm_medhs, na.rm = TRUE))
write.csv(CH4_means, "CH4_means_PCA.csv")

surveys %>% 
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            sd_weight = sd(weight, na.rm = TRUE))


# CO2
summary(GHG_2105$wCO2_uatm_medhs)    
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 418.5  4771.1  7279.1 12138.3 16963.3 48826.6

# CH4
summary(GHG_2105$wCH4_uatm_medhs)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -22.37   259.37  1629.73  6523.21  4854.98 50357.17

##### Plotting data ####

# [1] GHG concentrations across sites

# CH4- 202105

ggplot(GHG_2105_SW, aes(x = Site, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 50000)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2021-05_SW_CH4.jpg")

# CO2- 202105

ggplot(GHG_2105_SW, aes(x = Site, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 12000)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2021-05_SW_CO2.jpg")


# CH4- 202011

ggplot(GHG_2011_SW, aes(x = Site, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 50000)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2020-11_SW_CH4.jpg")

# CO2- 202011

ggplot(GHG_2011_SW, aes(x = Site, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 12000)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2020-11_SW_CO2.jpg")


# CH4 against CO2 for surface water
ggplot(GHG_2105_SW, aes(x= wCO2_uatm_medhs, y= wCH4_uatm_medhs, color = Site)) +
  geom_point() +
  theme_bw() + theme(legend.position = "right", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

# CH4 against CO2 for groundwater
ggplot(GHG_2105_GW, aes(x= wCO2_uatm_medhs, y= wCH4_uatm_medhs, color = Site)) +
  geom_point() +
  theme_bw() + theme(legend.position = "right", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))


# CO2 SW vs GW (2020-11)

ggplot(GHG_2011, aes(x = Sample_Type, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2020-11_CO2_Boxplot.jpg")

# CO2 SW vs GW (2020-11)

ggplot(GHG_2105, aes(x = Sample_Type, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CO2 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2021-05_CO2_Boxplot.jpg")

# CH4 SW vs GW (2020-11)

ggplot(GHG_2011, aes(x = Sample_Type, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2020-11_CH4_Boxplot.jpg")

# CH4 SW vs GW (2020-11)

ggplot(GHG_2105, aes(x = Sample_Type, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2021-05_CH4_Boxplot.jpg")


# Plotting GHG on ND SW-GW transect

# ND SW-GW: 2020-11

ggplot(GHG_2011_ND, aes(x= Site_ID, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
ggsave("Graphs/2021-05_ND_CH4.jpg")

ggplot(GHG_2011_ND, aes(x= Site_ID, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
ggsave("Graphs/2021-05_ND_CO2.jpg")

# ND SW-GW: 2021-05

ggplot(GHG_2105_ND, aes(x= Site_ID, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
ggsave("Graphs/2021-05_ND_CH4.jpg")

ggplot(GHG_2105_ND, aes(x= Site_ID, y= wCO2_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
ggsave("Graphs/2021-05_ND_CO2.jpg")


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