# Script for merging all dates
# Carla López Lloreda
# Created: 10/7/2021, updated: 11/8/2021

# Load libraries
library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)
library(ggplot2)

# Set working directory
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

#### Merging all dates ####

# Read in processed GHG data for all synoptic sampling dates
GHG_202011 <- read_csv("202011_GHG_clean.csv")
GHG_202102 <- read_csv("202102_GHG_clean.csv")
GHG_202105 <- read_csv("202105_GHG_Clean.csv")
GHG_202106 <- read_csv("202106_GHG_Clean.csv")
GHG_202109 <- read_csv("202109_GHG_Clean.csv")
# GHG_202111 <- <- read_csv("202111_GHG_Clean.csv")

# Merge all synoptic dates together
GHG_all <- rbind(GHG_202011, GHG_202102, GHG_202105)

ggplot(GHG_all, aes(x= Site_ID, y = CO2_uM)) +
  geom_boxplot()

ggplot(GHG_all, aes(x= Site_ID, y = CH4_uM)) +
  geom_boxplot()

