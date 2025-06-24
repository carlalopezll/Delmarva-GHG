#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 13 Jan 2021
# Purpose: Aggregate the raw downloads into a clean .csv for Jackson Lane 2022 Deployment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Libraries and functions 

library(xts)
library(dygraphs)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)

source("CO2/scripts/functions/download_fun.R")
source("CO2/scripts/functions/prelim_plot.R")

data_dir <- "CO2/data"

# 2. Read the JL files ----------------------------------------------

# Lists all files in data_dir
files <- list.files(paste0(data_dir), full.names = TRUE)
# Selects only the GP files
eosGP_files <- files[str_detect(files, "eosGP")]

# Run the download function and combine GP files
data <- eosGP_files %>% 
  map(download_fun) %>% 
  reduce(rbind)

rm(files, eosGP_files)

# 3. Remove the redundant data points and bad measurement times -----------------------------------------------

data <- data %>% 
  # Returns only the rows with unique values across. Eliminates overlapping data.
  distinct(Site_ID, TIMESTAMP, RECORD, .keep_all = TRUE) %>% 
  # Removes the wonky rows below column names
  filter(!TIMESTAMP == "TS") %>% 
  # Reformat columns accordingly
  transmute(Timestamp = ymd_hms(TIMESTAMP),
         BattV = as.numeric(BattV),
         Logger_TempC = as.numeric(PTemp_C),
         CO2_Conc_ppm = as.numeric(GP_CO2Conc),
         CO2_HiConc_ppm = as.numeric(GP_CO2HiConc),
         GP_TempC = as.numeric(GP_Temp),
         Site_ID = Site_ID,
         file = file) # Remove this column once data is clean 

#   3a. DK-SW cleaning ------------------------------------------------------

SiteName <- "DK"

# Create a dygraph to cut bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, CO2_HiConc_ppm))

prelim_plot(df %>% select(c(Timestamp, CO2_HiConc_ppm)))

# Filter out bad measurements based on dygraph
data_DK <- data %>% 
  filter(Site_ID == SiteName) %>%
  filter(CO2_HiConc_ppm > 1800) %>%
  filter(Timestamp < "2022-09-13 00:00" | Timestamp > "2023-04-17 00:00")

prelim_plot(data_DK %>% select(c(Timestamp, CO2_HiConc_ppm)))

#   3b. TS-SW cleaning ------------------------------------------------------

SiteName <- "TS"

# Create a dygraph to cut out bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, CO2_HiConc_ppm))

prelim_plot(df %>% select(c(Timestamp, CO2_HiConc_ppm)))

# Filter out bad values based on the dygraph
data_TS <- data %>% 
  filter(Site_ID == SiteName) %>%
  filter(CO2_HiConc_ppm > 1800) %>%
  filter(Timestamp < "2023-08-11 00:00")

prelim_plot(data_TS %>% select(c(Timestamp, CO2_HiConc_ppm)))

#   3c. ND-SW cleaning ------------------------------------------------------

SiteName <- "ND"

# Create a dygraph to cut bad data points
df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  select(c(Timestamp, CO2_HiConc_ppm))

prelim_plot(df %>% select(c(Timestamp, CO2_HiConc_ppm)))

data_ND <- data %>% 
  filter(Site_ID == SiteName) %>%
  filter(CO2_HiConc_ppm > 1800) %>%
  filter(Timestamp <"2023-06-05 00:00" | Timestamp > "2023-06-11")

prelim_plot(data_ND %>% select(c(Timestamp, CO2_HiConc_ppm)))

# 3d. Combine JL sites -------------------------------------------------------

# Merge sites back together AFTER bad values are cut
data_cleaned <- rbind(data_DK, data_ND, data_TS)

theme <- theme(axis.text = element_text(size = "18"), axis.title = element_text(size=20))

gg_cleaned <- ggplot(data = data_cleaned, 
                     mapping = aes(x = Timestamp,
                                   y = CO2_HiConc_ppm, 
                                   color = Site_ID)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "4 months") +
  theme +
  labs(x= "Timestamp", y = "CO2 (ppmv)")

gg_cleaned

# 4. Convert time zone from EDT to EST at JL ---------------------------------------------------

hrs <- hours(1)
# Subtract an hour to convert all data to EST
data_cleaned_EST <- data_cleaned %>% 
  mutate(Timestamp = Timestamp - hrs)

# XX. Write csv -----------------------------------------------------------

write_csv(data_cleaned, file = paste0("CO2/data/processed data/eosGP_JL_Raw.csv"))
