# Script for merging high-frequency sensor data from Delmarva
# Sources: CO2, DO, precipitation, water level

# Merging gives a crazy spreadsheet, need to improve this workflow - CLL 6/10/2025

library(ggpubr) # for stats in graphs
library(broom)
library(readr)
library(dplyr)
library(lubridate)
library(ggpubr) # for stats in plots
library(data.table)

# Read in datasets

# wl <- read_csv("Water level & precip/data/WL_metrics.csv")
wl <- read_csv("Hydrology/data/WL_JL_250403.csv")
do <- read_csv("DO/merged data/DO_JL_Raw.csv")
co2 <- read_csv("CO2/data/processed data/eosGP_JL_clean.csv")
precip <- read_csv("Hydrology/data/Precip_JL.csv")

source("CO2/scripts/0-setup.R")

# Round all datasets to 15 minutes

do$Timestamp <- round_date(do$Timestamp, "15 minutes")
wl$timestamp_corrected <- round_date(wl$Timestamp_corrected, "15 minutes")
co2$Timestamp <- round_date(co2$Timestamp, "15 minutes")

# Create Site_ID column for water level
wl$Site_ID <- substring(wl$Site_Name, 1,2)

library(dplyr)
library(purrr)

# 1. Separar por Site_ID
co2_split <- split(co2, co2$Site_ID)

# 2. Hacer merge de cada subset con precip por timestamp
co2_merged_list <- map(co2_split, ~ left_join(.x, precip, by = c("Timestamp" = "timestamp")))

# 3. Volver a unir todos los dataframes
co2_final <- bind_rows(co2_merged_list)

co2_final <- as.data.table(co2_final)
do <- as.data.table(do)
wl <- as.data.table(wl)

# Make sure Site_ID exists in co2, do, wl
co2_final[, Site_ID := substring(Site_ID, 1, 2)]
do[, Site_ID := substring(Site_ID, 1, 2)]
wl[, Site_ID := substring(Site_ID, 1, 2)]

# Make sure all timestamps are POSIXct
co2_final[, timestamp := as.POSIXct(Timestamp)]
do[, timestamp := as.POSIXct(Timestamp)]
wl[, timestamp := as.POSIXct(Timestamp_corrected)]

# # Precip doesn't have site info, so just join by timestamp FIRST
# setkey(precip, timestamp)
# setkey(co2_final, timestamp)
# sensors <- precip[co2, on = .(timestamp), roll = TRUE]

# Set keys for site-aware joins
setkey(wl, timestamp, Site_ID)
setkey(do, timestamp, Site_ID)
setkey(co2_final, timestamp, Site_ID)

# Rolling join wl and do (now with Site_ID)
sensors <- wl[co2_final, on = .(timestamp, Site_ID), roll = TRUE]
sensors <- do[sensors, on = .(timestamp, Site_ID), roll = TRUE]













library(data.table)
library(zoo)

# Get full range of timestamps across all datasets
start_time <- min(wl$timestamp, na.rm = TRUE)
end_time <- max(wl$timestamp, na.rm = TRUE)
full_time <- seq(from = start_time, to = end_time, by = "15 min")

# Get unique Site_IDs
sites <- unique(wl$Site_ID)

# Build empty data.table with all Site_IDs and full 15-min timestamps
grid <- CJ(timestamp = full_time, Site_ID = sites)  # CJ = Cross Join in data.table
setkey(grid, timestamp, Site_ID)

# Merge actual WL data into the grid
setkey(wl, timestamp, Site_ID)
wl_interp <- wl[grid, on = .(timestamp, Site_ID)]

# Get names of numeric columns, but exclude timestamp and Site_ID
num_cols <- names(wl_interp)[sapply(wl_interp, is.numeric) & 
                               !(names(wl_interp) %in% c("timestamp", "Site_ID"))]

# Apply interpolation to only those columns
wl_interp[, (num_cols) := lapply(.SD, function(x) na.approx(x, x = timestamp, na.rm = FALSE)),
          by = Site_ID, .SDcols = num_cols]

setkey(wl_interp, timestamp, Site_ID)
setkey(sensors, timestamp, Site_ID)

sensors <- wl_interp[sensors, on = .(timestamp, Site_ID)]

# Extracting month column
sensors$month <- lubridate::month(sensors$Timestamp, label = TRUE, abbr = FALSE)

# Extract month from timestamp
sensors$month_num <- as.numeric(format(sensors$Timestamp, "%m"))

# Define a function to get the season based on month
get_season <- function(month) {
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  } else {
    return("Winter")
  }
}

# Get the season
sensors$season <- sapply(sensors$month_num, get_season)

write.csv(sensors, "CO2/data/processed data/merged sensors_250412.csv")
