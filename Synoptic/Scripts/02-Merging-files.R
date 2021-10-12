# Script for merging all variables
# and all dates

# This will be important for PCA

# Carla López Lloreda
# 10/7/2021


#### Merging all dates

# Read in processed GHG data for all synoptic sampling dates
GHG_2105_complete <- read_csv("2021-05/202105_GHG_Wetlands.csv")
GHG_2011_complete <- read_csv("2020-11/202011_GHG_Wetlands.csv")