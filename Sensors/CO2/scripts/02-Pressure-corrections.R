# Correcting eosGP data for pressure
# Based on eosense instructions
# Data is already temperature corrected

# Created by Carla L?pez Lloreda for Delmarva CO2 sensor data
# Last updated 4/8/2023

# Install libraries
library(dplyr)
library(tidyr)
library(readr)

# 1. Correct for pressure
    # When using the eosGP in air or soil,
    # the concentrations must be corrected by using the
    # pressure at the time of measurement and the calibration
    # (reference) pressure.

# CO2 corrected = CO2 measured * (Pressure reference / (Pressure measured atmospheric + Pressure hydrostatic))
# Pressure reference = 101.3kPa

# Atmospheric pressure is a site average from YSI barometric pressure measurements

P_ref = 101.3 # kPa
P_atm = 75.8 # kPa

# When measuring in water, you need to account for hydrostatic pressure as well
# P = pgh
# p = density of water (1000 k g m^3)
# g = gravitational acceleration (9.8 m^2/s)
# h = height of the water column

# Our sensors are installed at ~3 inches = 0.0762m (need to confirm this with James)

P_hydro = (1000 * 9.8 * 0.0762)/1000 # units are in kPa

# Get sensor data
sensors <- read_csv("CO2/data/processed data/eosGP_JL_Raw.csv")

# CO2 corrected = CO2_ppmv * (P_ref (84.1 + P_hydro))

sensors$CO2_ppm_corr <- sensors$CO2_HiConc_ppm * (P_ref/(P_atm + P_hydro))

sensors$pCO2_kpa <- sensors$CO2_ppm_corr * (P_atm + P_hydro) * 10^-6

# Convert pCO2 to concentrations
# Haven't figured this out yet

# CO2 (M) = pCO2/KH

# KH = 

##### [1] SOLUBILITY CONSTANT FOR CO2 #####
# as in Weiss 1974 for units of mol L-1 atm-1 (also in Demarty et al 2011; many others)

# KH.CO2 <- function(tempC){
#   tempK <- tempC + 273.15
#   KH.CO2 <- exp( -58.0931 + (90.5069*(100/tempK)) + 22.2940*log((tempK/100), base=exp(1)) )
#   KH.CO2
# }
# 
# log KH = 108.3865 + 0.01985076T - 6919.53/T - 40.45154 log T + 669365./T*
# 
# KH.CO2(tempC = 10)
# 
# sensors$CO2_M <- sensors$pCO2_kpa/(KH.CO2(tempC = sensors$GP_TempC)*101.325)

# For now, just save this

write_csv(sensors, "CO2/processed data/eosGP CO2_pressure corrected.csv")
