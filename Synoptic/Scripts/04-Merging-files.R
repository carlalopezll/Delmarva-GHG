# Script for merging all dates
# Carla López Lloreda
# Updated: 10/27/2023

# Need to: make a merged dataset for non-averaged files (ie. not Clean ones)

# Load libraries
library(plyr)
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

#### Merging GHG lab data ####

# Merging all dates

# Read in processed CLEAN GHG data for all synoptic sampling dates (NEON pipeline)
GHG_202011 <- read.csv("Clean data/NEON/202011_GHG_NEON_clean.csv")
GHG_202102 <- read.csv("Clean data/NEON/202102_GHG_NEON_clean.csv")
GHG_202105 <- read.csv("Clean data/NEON/202105_GHG_NEON_clean.csv")
GHG_202106 <- read.csv("Clean data/NEON/202106_GHG_NEON_clean.csv")
GHG_202109 <- read.csv("Clean data/NEON/202109_GHG_NEON_clean.csv")
GHG_202110 <- read.csv("Clean data/NEON/202110_GHG_NEON_clean.csv")
GHG_202111 <- read.csv("Clean data/NEON/202111_GHG_NEON_clean.csv")
GHG_202112 <- read.csv("Clean data/NEON/202112_GHG_NEON_clean.csv")
GHG_202203 <- read.csv("Clean data/NEON/202203_GHG_NEON_clean.csv")
GHG_202210 <- read.csv("Clean data/NEON/202210_GHG_NEON_clean.csv")
GHG_202212 <- read.csv("Clean data/NEON/202212_GHG_NEON_clean.csv")
 
# Merge GHG data for all synoptic dates together
GHG_all <- rbind(GHG_202011, GHG_202102, GHG_202105, GHG_202106, 
                 GHG_202109, GHG_202110, GHG_202111, GHG_202112, 
                 GHG_202203, GHG_202210, GHG_202212)

# Fixing dates

GHG_all$Sample_Date <- as.POSIXct(strptime(as.character(GHG_all$Sample_Date,"%d/%m/%Y"), format = "%Y%m%d"))

# Creating month and year column

GHG_all <- GHG_all %>%
  filter(!is.na(Sample_Type)) %>%
  mutate(month = month(Sample_Date), year = year(Sample_Date))

# Creating a "watershed" variable

GHG_all$watershed <- ifelse(GHG_all$Site == "ND" | GHG_all$Site == "BD" |
                              GHG_all$Site == "TS" | GHG_all$Site == "DK" | GHG_all$Site == "FR", GHG_all$watershed <- "Jackson Lane", 
                            ifelse(GHG_all$Site == "TA" | GHG_all$Site == "TB" | GHG_all$Site == "DB" |
                                     GHG_all$Site == "FN", GHG_all$watershed <- "Tiger Paw",
                                   ifelse(GHG_all$Site == "JA" | GHG_all$Site == "JB" |
                                            GHG_all$Site == "JC" | GHG_all$Site == "NB", GHG_all$watershed <- "Jones Road" , 
                                          ifelse(GHG_all$Site == "OB" | GHG_all$Site == "XB" |
                                                   GHG_all$Site == "MB" | GHG_all$Site == "HB", GHG_all$watershed <- "Baltimore Corner", 
                                                 ifelse(GHG_all$Site == "TI" | GHG_all$Site == "QB" | GHG_all$Site == "DF", GHG_all$watershed <- "Baltimore Corner", NA)))))

# Creating a "wetland complex" variable

GHG_all$wetland_complex <- ifelse(GHG_all$Site == "ND" | GHG_all$Site == "BD" |
                              GHG_all$Site == "TS" | GHG_all$Site == "DK" | GHG_all$Site == "FR" | 
                              GHG_all$Site == "TA" | GHG_all$Site == "TB" | GHG_all$Site == "DB" |
                                GHG_all$Site == "FN", GHG_all$wetland_complex <- "Jackson Lane",
                                   ifelse(GHG_all$Site == "JA" | GHG_all$Site == "JB" | GHG_all$Site == "JC" | GHG_all$Site == "NB" | 
                                          GHG_all$Site == "OB" | GHG_all$Site == "XB" |
                                          GHG_all$Site == "MB" | GHG_all$Site == "HB", GHG_all$wetland_complex <- "Baltimore Corner",
                                                 ifelse(GHG_all$Site == "TI" | GHG_all$Site == "QB" | GHG_all$Site == "DF", GHG_all$wetland_complex <- "Baltimore Corner", NA)))


# Rearrange factor order by site/watershed

GHG_all$Site <- factor(GHG_all$Site,levels = c("ND", "BD", "TS", "DK", "FR", # Jackson Lane
                                               "TA", "TB", "DB", "FN", # Tiger Paw/Beetree Rd (Jackson Lane)
                                               "JA", "JB", "JC", "NB", # Jones Rd N (Baltimore Corner)
                                               "OB", "XB", "MB", "HB", "TP", # Baltimore Corner
                                               "TI", "QB", "DF", # Jones Rd S (Baltimore Corner)
                                               "CR", "TR", "AG")) # Rivers, own name for watershed

write.csv(GHG_all, "All_GHG_NEON.csv", row.names = FALSE)

#### Merging field data ####

# These NEED TO have the same columns

field_202011 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202011.csv")
field_202102 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202102.csv")
field_202105 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202105.csv")
field_202106 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202106.csv")
field_202109 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202109.csv") # has atm pressure
field_202110 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202110.csv")
field_202111 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202111.csv")
field_202112 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202112.csv")
field_202203 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202203.csv")
field_202210 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202210.csv")
field_202212 <- read.csv("Digitized Field Notes/Digitized_Field_Data_202212.csv")


# Merge all digitized field sheets

field <- rbind.fill(field_202011, field_202102, field_202105, field_202106, 
                    field_202109, field_202110, field_202111, field_202112, 
                    field_202203, field_202210, field_202212)

# Fixing date for field data

field$Date <- as.POSIXct(strptime(as.character(field$Date,"%d/%m/%Y"), format = "%Y%m%d"))

# Save merged field data

write.csv(field, "All_field.csv", row.names = FALSE)

#### Merge GHG data with field data ####

# If not changing anything to the above, just load this

field <- read.csv("All_field.csv")
GHG_all <- read.csv("All_GHG_NEON.csv")

#### Merging GHG and field data ####

GHG_field <- merge(GHG_all, field, by.x=c("Site_ID", "Sample_Date"), by.y=c("Site_ID", "Date"), all.x = TRUE)

# Replace field data column names
colnames(GHG_field)[14] <- "SpC"
colnames(GHG_field)[15] <- "DO_percent"
colnames(GHG_field)[16] <- "DO_mgL"
colnames(GHG_field)[17] <- "Temp_C"
colnames(GHG_field)[19] <- "Field_flag"
colnames(GHG_field)[21] <- "Grab_flag"
colnames(GHG_field)[23] <- "Atm_pressure_mBar"

# Remove NAs

GHG_field <- GHG_field[!is.na(GHG_field$Sample_Date), ]

# Save merged GHG and field data as csv

write.csv(GHG_field, "GHG_field.csv", row.names = FALSE)

# If not adding anything new, load GHG + field data

GHG_field <- read.csv("GHG_field.csv")

#### Adding rest of the synoptic data ####

# synoptic <- read.csv("lab_data_aggregated_JM_wide.csv")
synoptic <- read.csv("lab_data_aggregated_JM_summary_wide.csv") # is this the most up to date??

# Correct dates

GHG_field$Date_corrected <- as.Date(parse_date_time(GHG_field$Sample_Date, c("mdy", "ymd")))

synoptic$Date_corrected <- as.Date(parse_date_time(synoptic$Sample_Date, c("mdy", "ymd")))

# Merge GHG + collated synoptic

merge <- merge(GHG_field, synoptic, by.x=c("Site_ID", "Date_corrected"), 
                                    by.y=c("Site_ID", "Date_corrected"), all.x = TRUE)

#### Adding new columns and ratio variables ####

# Getting yymm column

merge$yymm <- format(merge$Date_corrected, format = "%Y-%m")

# Changing 2020-11 into 2020-10 so they plot on the same category in the boxplots


# Changing 2020-11 into 2020-10 so they plot on the same category in the boxplots
merge$yymm_new = if_else(merge$yymm == "2020-11", merge$yymm_new <- "2020-10", merge$yymm_new <- merge$mmyy)
merge$yymm_new = if_else(merge$yymm == "2022-04", merge$yymm_new <- "2022-03", merge$yymm_new <- merge$yymm)

# Calculating deuterium excess
merge$d <- merge$d2H - 8 * merge$d18O

# Redox ratios

merge$CH4_CO2 <- merge$CH4_uM/merge$CO2_uM
merge$NH4_NO3 <- merge$NH3/merge$NO3


# Save merged file - THIS IS THE FILE WITH EVERYTHING EXCEPT MORPH CHARACTERISTICS

write.csv(merge, "Merged_synoptic.csv", row.names = FALSE)

#### Adding wetland morphological variables ####

wetland_info <- read.csv("wetland_info_table.csv") # sheet with wetland characteristics

# Add wetland characteristics to merge dataframe

merge_morph <- left_join(merge, wetland_info, by = c("Site" = "site_ID"))

# Save merged file - THIS IS THE FILE WITH SYNOPTIC + MORPH CHARACTERISTICS

write.csv(merge_morph, "Merged_synoptic_morph.csv", row.names = FALSE)



#### Adding water level ####

merge_morph <- read.csv("Merged_synoptic_morph.csv")

# Use the water level daily mean
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Water level")
daily_wl <- read.csv("dly_mean_output_JM_2019_2022.csv")

# Adding a site column
daily_wl$Site <- substr(daily_wl [ , 2], start= 1, stop= 2) # Make sure that you have the right column for Site_ID

# Adding well type (GW, SW or CH)
daily_wl$Sample_Type <- substr(daily_wl [ , 2], start= 4, stop = 5) # Make sure that you have the right column for Site_ID

# Filter out flags

daily_wl <- filter(daily_wl, Flag == "0")


# Bring together with water chem

wl_chem <- merge(merge_morph, daily_wl, by.x = c("Site", "Site_ID", "Date_corrected", "Sample_Type"), by.y = c("Site", "Site_ID", "Date", "Sample_Type"), all.x=TRUE, all.y = FALSE)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

write.csv(wl_chem, "Master spreadsheet.csv")
