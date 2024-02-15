# Replacing missing temperatures for Delmarva synoptic data

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Replacing work flow below

# Merge GCHeadspace data with temp data from Digitized field sheets

hs <- read.csv("2020-01_Erin/202001_GHG_GCHeadspace.csv")
field <- read.csv("Digitized Field Notes/Digitized_Field_Data_202001.csv")




#   # Getting averages for 2020-01
#   # Using 2021-02 data as a comparison
# 
# GHG_202001 <- read.csv("2020-01_Erin/202001_GHG_GCHeadspace.csv")
# GHG_202102 <- read.csv("2021-02/202102_GHG_GCHeadspace.csv")
# 
# GHG_202102_temp <- GHG_202102 %>%
#   filter(!Rep == "Air") %>%
#   group_by(Site_ID) %>%
#   summarise(WaterT_C_avg = mean(WaterT_C))
# 
# GHG_202001_temp <- left_join(GHG_202001, GHG_202102_temp, by = "Site_ID")
# GHG_202001_temp$WaterT_C <- GHG_202001_temp$WaterT_C_avg
# 
# # QB-Edge missing temp but could just use QB-SW
# 
# write.csv(GHG_202001_temp, "2020-01_Erin/202001_GHG_GCHeadspace_wTemp.csv")
# 
#   # Getting averages for 2020-03
#   # Using 2021-02 data as comparison
# 
# GHG_202003 <- read.csv("2020-03_Erin/202003_GHG_GCHeadspace.csv")
# GHG_202102 <- read.csv("2021-02/202102_GHG_GCHeadspace.csv")
# 
# GHG_202102_temp <- GHG_202102 %>%
#   filter(!Rep == "Air") %>%
#   group_by(Site_ID) %>%
#   summarise(WaterT_C_avg = mean(WaterT_C))
# 
# GHG_202003_temp <- left_join(GHG_202003, GHG_202102_temp, by = "Site_ID")
# GHG_202003_temp$WaterT_C <- GHG_202003_temp$WaterT_C_avg
# 
# write.csv(GHG_202003_temp, "2020-03_Erin/202003_GHG_GCHeadspace_wTemp.csv")
# 
# 
#   # Getting averages for 2020-07
#   # Using 2021-06 as comparison
# 
# GHG_202007 <- read.csv("2020-07_Erin/202008_GHG_GCHeadspace.csv")
# GHG_202106 <- read.csv("2021-06/202106_GHG_GCHeadspace.csv")
# 
# GHG_202106_temp <- GHG_202106 %>%
#   filter(!Rep == "Air") %>%
#   group_by(Site_ID) %>%
#   summarise(WaterT_C_avg = mean(WaterT_C))
# 
# GHG_202007_temp <- left_join(GHG_202007, GHG_202106_temp, by = "Site_ID")
# GHG_202007_temp$WaterT_C <- GHG_202007_temp$WaterT_C_avg
# 
# write.csv(GHG_202007_temp, "2020-07_Erin/202008_GHG_GCHeadspace_wTemp.csv")
# 
