#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# James Maze, modified by Carla López Lloreda for use with the miniDOT DO data
# Date: 10/24/2024
# Purpose: Aggregate the raw downloads into a clean .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Libraries and functions 

library(xts)
library(dygraphs)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)

source("DO/scripts/functions/download_fun.R")
source("DO/scripts/functions/prelim_plot.R")

data_dir <- "DO/raw data"

# 2. Read the JL files ----------------------------------------------

# Lists all files in data_dir
files <- list.files(paste0(data_dir), full.names = TRUE)
# Selects only the GP files
PME_files <- files[str_detect(files, "PME")]

# Run the download function and combine GP files
data <- PME_files %>% 
  map(download_fun) %>% 
  bind_rows()
  # reduce(rbind)

rm(files, PME_files)

# 3. Remove the redundant data points and bad measurement times -----------------------------------------------

data <- data %>% 
  # Returns only the rows with unique values across. Eliminates overlapping data.
  distinct(Site_ID, `               UTC_Date_&_Time`, .keep_all = TRUE) %>% 
  # Removes the wonky rows below column names
  slice(-1) %>%
  # Reformat columns accordingly
  transmute(Timestamp = ymd_hms(`               UTC_Date_&_Time`),
            BattV = as.numeric(`       Battery`),
            Logger_TempC = as.numeric(`   Temperature`),
            DO_conc_mgL = as.numeric(`  Dissolved Oxygen`),
            DO_perc = as.numeric(`  Dissolved Oxygen Saturation`),
            Q = as.numeric(`             Q`),
            Site_ID = Site_ID,
            file = file) %>% # Remove this column once data is clean
  drop_na(Timestamp)

# 4. Convert time zone from EDT to EST at JL ---------------------------------------------------

hrs <- hours(5)
# Subtract an hour to convert all data to EST
data <- data %>% 
  mutate(Timestamp = Timestamp - hrs)

# 3a. DK-SW cleaning ------------------------------------------------------

SiteName <- "DK"

# Create a dygraph to cut bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, DO_conc_mgL))

prelim_plot(df %>% select(c(Timestamp, DO_conc_mgL)))

#   3b. TS-SW cleaning ------------------------------------------------------

SiteName <- "TS"

# Create a dygraph to cut out bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, DO_conc_mgL))

prelim_plot(df %>% select(c(Timestamp, DO_conc_mgL)))

#   3c. ND-SW cleaning ------------------------------------------------------

SiteName <- "ND"

# Create a dygraph to cut bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, DO_conc_mgL))

prelim_plot(df %>% select(c(Timestamp, DO_conc_mgL)))

# 3d. Plot all sites -------------------------------------------------------

gg <- ggplot(data = data, aes(x = Timestamp, y = DO_conc_mgL, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "4 months") +
  labs(x= "Timestamp", y = "DO (mg/L)")

gg

# XX. Write csv -----------------------------------------------------------

write_csv(data, file = paste0("DO/raw data/DO_JL_Raw.csv"))

