# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)

# Set your working directory to where the files are stored
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Sensors/Light")  # <- Change this to your actual path

library(dplyr)
library(readr)
library(stringr)

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
    rename(row = X1,
           datetime = X2,
           temp_C = X3,
           lux = X4) %>%
    mutate(site = site_prefix)
  
  return(df)
}

# List all CSV files
file_list <- list.files(pattern = "*.csv", full.names = TRUE)

# Apply function to all files and merge
merged_data <- lapply(file_list, process_file) %>%
  bind_rows()

merged_data$datetime_corrected <- as.POSIXct(merged_data$datetime, format = "%m/%d/%y %I:%M:%S %p")

light <- merged_data

# Converting lux to PAR

# PPFD (µmol m⁻² s⁻¹) ≈ Lux * 0.018

light$PAR_umol_m2_d <- (light$lux * 0.018)/86400

ggplot(light, aes(x=datetime_corrected, y = PAR)) +
  geom_line() +
  facet_wrap(~site, ncol= 1)

summary <- light %>%
  group_by(site) %>%
  summarize(PAR_mean = mean(PAR_umol_m2_d, na.rm = TRUE)*10000)

# Optional: write to CSV
# write_csv(merged_data, "merged_with_site_prefix.csv")


