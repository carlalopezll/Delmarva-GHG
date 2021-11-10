# Looking at temp averages for missing temp data

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)

# Getting averages for 2020-01
# Using 2021-02 data as a comparison
GHG_202001 <- read.csv("2020-01_Erin/202001_GHG_GCHeadspace.csv")
GHG_202102 <- read.csv("2021-02/202102_GHG_GCHeadspace.csv")

GHG_202102_temp <- GHG_202102 %>%
  filter(!Rep == "Air") %>%
  group_by(Site_ID) %>%
  summarise(WaterT_C_avg = mean(WaterT_C))

GHG_202001_temp <- left_join(GHG_202001, GHG_202102_temp, by = "Site_ID")
GHG_202001_temp$WaterT_C <- GHG_202001_temp$WaterT_C_avg

# Getting averages for 2020-03
# Using 2021-02 data as comparison


# Getting averages for 2020-07
# Using 2021-06 as comparison

