# Script that merges all the light files and converts lux to PAR
# Carla López Lloreda

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

# Set your working directory to where the files are stored
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Sensors/Light/data/raw data")  # <- Change this to your actual path

# Function to process each file
process_file <- function(filepath) {
  # Extract site prefix from filename
  filename <- basename(filepath)
  site_prefix <- substr(filename, 1, 2)
  
  # Read file skipping first 1 rows
  df <- read_csv(filepath, skip = 2, col_names = FALSE, show_col_types = FALSE)
  
  # Keep only first 3 columns and rename
  df <- df %>%
    select(1:4) %>%
    rename(rows = X1,
           datetime = X2,
           temp_C = X3,
           lux = X4) %>%
    mutate(site = site_prefix)
  
  return(df)
}

# List all CSV files
file_list <- list.files(pattern = "*.csv", full.names = TRUE)

# Apply function to all files and merge
light <- lapply(file_list, process_file) %>%
  bind_rows()

light$datetime_corrected <- as.POSIXct(light$datetime, format = "%m/%d/%y %I:%M:%S %p")

# Converting lux to PAR
# PPFD (µmol m⁻² s⁻¹) ≈ Lux * 0.018

light$PAR <- (light$lux * 0.018) # in µmol m⁻² s⁻

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Sensors/Light/data")

# Write to csv
write_csv(light, "light_data.csv")


