# Load libraries

library(udunits2) # unit conversions
library(scales)
library(methods)
library(gridExtra)
library(plotly)
library(tidyr)

# Set up

source("CO2/scripts/0-setup.R")

color_palette <- c("DK" = "#F8766D", 
                   "ND" = "#00BA38", 
                   "TS" = "#619CFF")

sensors <- read_csv("CO2/data/merged sensors_250412.csv")

# Reordering seasons
sensors$season <- factor(sensors$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Reordering site IDs
sensors$Site_ID <- factor(sensors$Site_ID, levels = c("TS", "DK", "ND"))

# Filter so that we use only the time with precip and CO2 data

sensors <- filter(sensors, timestamp < "2022-08-04")


# Rename sites

sensors <- sensors %>%
  mutate(Site_hydroperiod = recode(Site_ID, 
                                   TS = "Short", 
                                   DK = "Medium", 
                                   ND = "Long"))

# Defining k
# Need to do it for each site but for now using ND

# Equilibrated CO2 (mol/m^3) = pCO2 (atm) * kH
# kH is temperature dependent

sensors <- sensors %>%
  mutate(k_600 = 0.3,
         CO2_eq_uM = (0.0004 * exp(9345 / (273.15 + GP_TempC) - 60.2409 + 23.3585 * log((273.15 + GP_TempC) / 100)))*1e6)

sensors <- sensors %>%
  mutate(k_600 = 0.3,
         K0 = exp(9345 / (273.15 + GP_TempC) - 60.2409 + 
                    23.3585 * log((273.15 + GP_TempC) / 100)),  # mol/(L·atm)
         CO2_eq_uatm = 420 / K0)  # uatm


sensors <- sensors %>%
  mutate(CO2_flux = k_600* (CO2_cal_uatm - CO2_eq_uatm))
