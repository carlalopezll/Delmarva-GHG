#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Baltimore Corner
# Coder: James Maze
# Date: 20 Jan 2021
# Purpose: Instrumentation problems at Baltimore Corner and the raw data is messy. Clean up.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Libraries and work space ----------------------------------------------

remove(list = ls())

library(xts)
library(dygraphs)
library(lubridate)
library(tidyverse)
library(stringr)

source("functions/prelim_plot.R")

data_dir <- "data/2021/"

# 2. Read in the data -----------------------------------------------------

#Read in the files without HB-SW
#Only files have nasty formatting. Do not run the download_fun
data_exHB <- read_csv(paste0(data_dir,"BC_CO2_20210802_20211217.csv")) %>% 
  mutate("Timestamp" = mdy_hm(`Measurement Time`)) %>% 
  select(-c(`Measurement Time`, )) %>% 
  rename("XB_SW_CO2" = `Gnarly Bay - 145C CO2`) %>% 
  rename("OB_SW_CO2" = `Origin Bay - 145B CO2`) %>% 
  rename("TP_CH_CO2" = `Outlet - 1458 CO2`) %>% 
  rename("MB_SW_CO2" = `Mixing Bay - 1454 CO2`)

#Read the full IWT file with HB-SW. Discard the other columns
data_HB <- read_csv(paste0(data_dir, "BC_all_sensors_20210802_20211217.csv")) %>% 
  select(c(`Measurement Time`, `Hummock Bay - 1459 CO2`)) %>% 
  mutate("Timestamp" = mdy_hm(`Measurement Time`)) %>% 
  rename("HB_SW_CO2" = `Hummock Bay - 1459 CO2`)

# 3. Make an dygraph showing all the BC data -----------------------------------------


data_exHB <- data_exHB %>% 
  pivot_longer(cols = c(XB_SW_CO2, OB_SW_CO2, TP_CH_CO2, MB_SW_CO2),
               names_to = "Site_ID",
               values_to = "CO2_ppm") %>% 
  filter(!is.na(CO2_ppm)) %>% 
  filter(!Site_ID == "MB_SW_CO2")


# Plot 
all_sites <- ggplot(data = data_exHB,
                    mapping = aes(x = Timestamp,
                                  y = CO2_ppm, 
                                  color = Site_ID)) +
             geom_point() +
  theme_bw()

(all_sites)

# 4. HB-SW ----------------------------------------------------------------

data_HB <- data_HB %>% 
  filter(!is.na(HB_SW_CO2)) %>% 
  filter(HB_SW_CO2 > 1000)

HB_plot <- ggplot(data = data_HB, 
                  mapping = aes(x = Timestamp,
                                y = HB_SW_CO2)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 20000)) +
  ylab("HB-SW CO2 (ppm uncorrected)") 

(HB_plot)