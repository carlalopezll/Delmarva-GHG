# Script for plotting Delmarva dissolved GHG synoptic samples
# Carla López Lloreda
# Last updated 10/5/2021

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
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Data/Synoptic/Data")

# Read in processed GHG dara
DMV_GHG_complete <- read_csv("2021-05/202105_GHG_Wetlands.csv")

# Adding new ID columns ### Need to work on
# Site IDs (withouth the SW or UW)         ## Added manually for now
# Surface water vs groundwater
   
########## Subsetting the data ###########

# Subsetting by rows (filter) and columns (select)
filter(DMV_GHG, Site == "ND")

# Subsetting to include only site ID and GHG concentrations
DMV_GHG <- select(DMV_GHG_complete, Site_ID, wCO2_uatm_medhs, wCH4_uatm_medhs)

# Removing NAs
DMV_GHG <- na.omit(DMV_GHG)
# Subsetting by catchment
DMV_GHG_ND <- filter(DMV_GHG, Site_ID == "ND-SW" | Site_ID == "ND-UW1" | Site_ID == "ND-UW3")

# Plotting GHG on ND transect SW-GW transect
ggplot(DMV_GHG_ND, aes(x= Site_ID, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))



######## Graph some data ###########

ggplot(DMV_GHG, aes(x= wCO2_uatm_medhs, y= wCH4_uatm_medhs)) +
  geom_point() +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

### [1] GHG concentrations across sites

# CH4

ggplot(DMV_GHG, aes(x = Site, y= wCH4_uatm_medhs)) +
  geom_boxplot() +
  labs(x = "Site", y = "CH4 concentration (uatm)") + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/2021-05_CH4.jpg")



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