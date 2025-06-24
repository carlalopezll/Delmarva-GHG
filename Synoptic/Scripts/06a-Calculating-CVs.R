# Calculating coefficients of variation

library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(zoo)

# Read in if not adding any new data above
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Water level")
wl <- read.csv("output/output_JM_2019_2022.csv")

# Filter out flagged data

wl <- wl %>%
  filter(Flag == 0)

# This is the spreadsheet for daily average water level
# wl <- read_csv("output/dly_mean_output_JM_2019_2022.csv")

wl$Sample_Type <- substr(wl$Site_Name, start= 4, stop= 5)

# Fix timestamp
wl$Timestamp_corrected <- parse_date_time(wl$Timestamp, "Ymd HMS", truncated = 3)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

synoptic <- read.csv("Master spreadsheet.csv") # All the data

synoptic$Date_corrected <- as.Date(parse_date_time(synoptic$Date_corrected, c("mdy", "ymd")))

##### CVs for sites across time ####

# Creating new table only with needed variables for calculating CVs

CV_site <- 
  synoptic %>%
   group_by(Site_ID) %>%
   summarise(Sample_Type = first(Sample_Type), Site = first(Site),
             CH4_sd = sd(CH4_uM), CH4_mean = mean(CH4_uM),
             CO2_sd = sd(CO2_uM), CO2_mean = mean(CO2_uM),
             DO_sd = sd(DO_percent), DO_mean = mean(DO_percent))

CV_site$CV_CH4 <- (CV_site$CH4_sd/CV_site$CH4_mean)*100
CV_site$CV_CO2 <- (CV_site$CO2_sd/CV_site$CO2_mean)*100
CV_site$CV_DO <- (CV_site$DO_sd/CV_site$DO_mean)*100

# Calculate site CV for water level

CV_wl_site <- 
  wl %>%
  group_by(Site_Name) %>%
  summarise(Sample_Type = first(Sample_Type), Site = first(Site_Name),
            wl_sd = sd(waterLevel), wl_mean = mean(waterLevel))

CV_wl_site$CV_wl <- (CV_wl_site$wl_sd/CV_wl_site$wl_mean)*100

# Merge with CH4 and CO2 CV

CV_site <- left_join(CV_site, CV_wl_site, by = c("Site_ID" = "Site", "Sample_Type" = "Sample_Type"))

write.csv(CV_site, "Site CV_240828.csv")

#### CVs for dates across space ####

synoptic$yymm_new = if_else(synoptic$yymm == "2020-11", synoptic$yymm_new <- "2020-10", synoptic$yymm_new <- synoptic$mmyy)
synoptic$yymm_new = if_else(synoptic$yymm == "2022-04", synoptic$yymm_new <- "2022-03", synoptic$yymm_new <- synoptic$yymm)
                      
# Calculate time CV for SW

CV_time_SW <- synoptic %>%
  filter(Sample_Type == "SW") %>%
  group_by(yymm_new) %>%
  summarise(Sample_Type = first(Sample_Type),
            CH4_sd = sd(CH4_uM), CH4_mean = mean(CH4_uM), 
            CO2_sd = sd(CO2_uM), CO2_mean = mean(CO2_uM))

CV_time_SW$CV_CH4 <- (CV_time_SW$CH4_sd/CV_time_SW$CH4_mean)*100
CV_time_SW$CV_CO2 <- (CV_time_SW$CO2_sd/CV_time_SW$CO2_mean)*100

write.csv(CV_time_SW, "Time CV_SW.csv")

# Calculate time CV for GW

CV_time_UW <- synoptic %>%
  filter(Sample_Type == "UW") %>%
  group_by(yymm_new) %>%
  summarise(Sample_Type = first(Sample_Type),
            CH4_sd = sd(CH4_uM), CH4_mean = mean(CH4_uM), 
            CO2_sd = sd(CO2_uM), CO2_mean = mean(CO2_uM))

CV_time_UW$CV_CH4 <- (CV_time_UW$CH4_sd/CV_time_UW$CH4_mean)*100
CV_time_UW$CV_CO2 <- (CV_time_UW$CO2_sd/CV_time_UW$CO2_mean)*100

write.csv(CV_time_UW, "Time CV_UW.csv")
