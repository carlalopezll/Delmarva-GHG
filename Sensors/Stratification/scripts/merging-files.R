# 1. Libraries and functions 

library(xts)
library(dygraphs)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)

source("Stratification/scripts/functions/download_fun.R")
source("Stratification/scripts/functions/prelim_plot.R")

data_dir <- "Stratification/data"

# 2. Read the JL files ----------------------------------------------

# Lists all files in data_dir
files <- list.files(paste0(data_dir), full.names = TRUE)
# Selects only the GP files
PME_files <- files[str_detect(files, "temp")]

# Run the download function and combine GP files
data <- PME_files %>% 
  map(download_fun) %>% 
  bind_rows()
# reduce(rbind)

data$temp <- data$`Temp, °C (LGR S/N: 20069121, SEN S/N: 20069121)`

data$timestamp <- as_datetime(data$`Date Time, GMT-05:00`)

ggplot(data, aes(x= timestamp, y = `Temp, °C (LGR S/N: 20069121, SEN S/N: 20069121)`, color = Site_ID)) +
  geom_point()
