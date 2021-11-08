########################################
# Equations to used to estimate CO2 and CH4 from headspace sampling 
# Coded/checked/updated by ERH from UQAM/Krycklan files
# Code updated for Delmarva Project by ERH
# *** still need to: double-check uatm to umol/m3 conversions
# *** still need to: finalize best way to group air samples for headspace calcs
# *** still need to: decide what to do about site w/o temp
# *** still need to: fix CH4 calculations
# Last update: 20211029 by CLL
########################################

# Load libraries

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)

############# A. LOAD FUNCTIONS #############

##### [1] SOLUBILITY CONSTANT FOR CO2 #####
# as in Weiss 1974 for units of mol L-1 atm-1 (also in Demarty et al 2011; many others)
KH.CO2 <- function(tempC){
  tempK <- tempC + 273.15
  KH.CO2 <- exp( -58.0931 + (90.5069*(100/tempK)) +22.2940*log((tempK/100), base=exp(1)) )
  KH.CO2
}

##### [2] SOLUBILITY CONSTANT FOR CH4 #####
# same constants as in Demarty et al 2011 Biogeosciences, which references Lide 2007 Handbook of Chemistry and Physics
# units are mol L-1 atm-1
KH.CH4 <- function(tempC){
  tempK <- tempC + 273.15
  KH.CH4  <- exp(-115.6477 + (155.5756/((tempK)/100))+65.2553* log( ((tempK)/100), base=exp(1) ) -6.1698*((tempK)/100))*1000/18.0153
  KH.CH4
}

##### [3] FUNCTION TO ESTIMATE STREAM pCO2 from headspace sample data (what the GC gives you) ####
# temp inputs are in C
# pressure input is kPa IN LAB (used for molar volume at analysis)
# gasV and waterV units must be the same (to make equilibration headspace ratio)
# pCO2 of sample = what the GC or picarro gives you # uatm or ppmv
# pCO2 of hs = what you add to create headspace # uatm or ppmv
# 101.325 kPa = 1 atm
# 0.082057 = R = universal gas constant in L*atm / mol*K
StmCO2fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCO2.samp, pCO2.hs){
  tempLab.K <- tempLab.C + 273.15
  molV <- 0.082057*(tempLab.K)*(101.325/kPa) # L mol-1
  hsRatio <- gasV/waterV
  KH.Lab <- KH.CO2(tempLab.C) # mol L-1 atm-1
  KH.Site <- KH.CO2(tempSite.C) # mol L-1 atm-1
  StmCO2 <- (pCO2.samp*KH.Lab + (hsRatio*(pCO2.samp-pCO2.hs) / molV )  ) /KH.Site
  StmCO2
}

##### [4] FUNCTION TO CONVERT pCO2 from uatm to umol/m3 ####
# 1 m3 = 1000 L; convert umol/m3 to umol/L = Fw/1000
FwCO2 <- function(tempC, CO2w.uatm){
  # Parameters and units:
  # CO2w.uatm = uatm
  # assume uatm = ppmv
  # R = L atm / K mol
  R <- 0.08205601 # from Weiss 1974
  # Tk = K
  tempK <- tempC + 273.15
  # convert CO2, water (uatm) to umol/m3 
  CO2wM <- CO2w.uatm*(1/R)*(1/(10^-3))*(1/tempK)
  CO2wM
}

##### [5] FUNCTION TO ESTIMATE STREAM pCH4 from headspace sample data (what the Picarro or GC gives you) ####
# temp inputs are in C
# pressure input is kPa IN LAB (used for molar volume at analysis)
# gasV and waterV units must be the same (to make equilibration headspace ratio)
# pCH4 of sample = what the GC or picarro gives you # uatm or ppmv
# pCH4 of hs = what you add to create headspace # uatm or ppmv
# pkH = solubility at sampling temp = M atm-1
# 101.325 kPa = 1 atm
# 0.082057 = R = universal gas constant in L*atm / mol*K
StmCH4fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCH4.samp, pCH4.hs){
  tempLab.K <- tempLab.C + 273.15
  molV <- 0.082057*(tempLab.K)*(101.325/kPa) # L mol-1 calculated for lab conditions
  hsRatio <- gasV/waterV
  KH.Lab <- KH.CH4(tempLab.C) # mol L-1 atm-1
  KH.Site <- KH.CH4(tempSite.C) # mol L-1 atm-1
  StmCH4 <- (pCH4.samp*KH.Lab + (hsRatio*(pCH4.samp-pCH4.hs) / molV )  ) /KH.Site
  StmCH4
}

##### [6] FUNCTION TO CONVERT pCH4 from uatm to umol/m3 ####
# **** Need to check updated code specific to CH4 (not CO2) ****
# 1 m3 = 1000 L; convert umol/m3 to umol/L = Fw/1000
FwCH4 <- function(tempC, CH4w.uatm){
  # Parameters and units:
  # CH4w.uatm = uatm
  # assume uatm = ppmv
  # R = L atm / K mol
  R <- 0.08205601 # from Weiss 1974
  # Tk = K
  tempK <- tempC + 273.15
  # convert CO2, water (uatm) to umol/m3 
  CH4wM <- CH4w.uatm*(1/R)*(1/(10^-3))*(1/tempK)
  CH4wM
}


########################################

############# B. LOAD/MERGE DATA FILES #############

# LOAD files from (1) GC output, (2) lab notes with headspace volumes, and (3) any different site IDs

# setwd("/Users/erh82/Dropbox/WetlandConnectCarbon_NSF/data_ghg/") # **CHANGE WITH USERS**

# Carla working space
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read data for synoptic
GHG <- read.csv("2021-06/202106_GHG_GCHeadspace.csv") # **CHANGE FOR SAMPLING MONTH**

# Summarize air data for different sites; add column to data file with hsCO2_ppm & hsCH4_ppm (e.g., LabAir, JL Air)
# Air_Location is the ID that will match up with air-water

# Filter air samples, group by location and calculate median  ## add other stats and include CH4
air_summary <- GHG %>%
  filter(Rep == "Air") %>%
  group_by(Air_Location) %>%
  summarize(AirCO2_min_ppm = min(CO2_ppm, na.rm = TRUE), AirCO2_med_ppm = median(CO2_ppm, na.rm = TRUE), 
   AirCO2_max_ppm = max(CO2_ppm, na.rm = TRUE), AirCH4_min_ppm = min(CH4_ppm, na.rm = TRUE),
   AirCH4_med_ppm = median(CH4_ppm, na.rm = TRUE), AirCH4_max_ppm = max(CH4_ppm, na.rm = TRUE))

# Save GHG median to a csv
write.csv(air_summary, "2021-06/air_summary.csv")

# Adding summary air columns to GHG
GHG_new <- left_join(GHG, air_summary, by = "Air_Location")

########################################

############# C. CONVERT GC DATA TO STREAM CO2/CH4 #############

# Estimate stream CO2/CH4 from GC headspace (uatm) 
# NOTE: lab temp and pressure are fixed for now, 20C and 102kPa

# Read in data again with appended columns of avg air concentrations
GHG <- read.csv("2021-05/202105_GHG_GCHeadspace.csv")

# subset the data to exclude air samples
samp <- GHG[ which(GHG$Rep!="Air"),]

# NAs for sites without a site temp. Need to decide on a system for replacing Temp NAs with a median site value or something else....

# StmCO2fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCO2.samp, pCO2.hs)
samp$wCO2_uatm_medhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_med_ppm)
samp$wCO2_uatm_minhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_min_ppm)
samp$wCO2_uatm_maxhs <- StmCO2fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCO2.samp=samp$CO2_ppm, pCO2.hs=samp$AirCO2_max_ppm)

# StmCH4fromSamp <- function(tempLab.C, tempSite.C, kPa, gasV, waterV, pCH4.samp, pCH4.hs)
samp$wCH4_uatm_medhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_med_ppm)
samp$wCH4_uatm_minhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_min_ppm)
samp$wCH4_uatm_maxhs <- StmCH4fromSamp(tempLab.C=20, tempSite.C=samp$WaterT_C, kPa=102, gasV=samp$AirV_mL, waterV=samp$WaterV_mL, pCH4.samp=samp$CH4_ppm, pCH4.hs=samp$AirCH4_max_ppm)
# VALUES ARE WAYYY TOO HIGH - need to revisit...done for now /E *****************************
  
#### CONVERT pCO2 and pCH4 from uatm to umol/m3 ** check on these conversions, especially for CH4! ** ####
# Need to finalize - CLL

samp$wCO2_umolm3_med <- FwCO2(tempC = samp$WaterT_C, CO2w.uatm = samp$wCO2_uatm_medhs)
samp$wCH4_umolm3_med <- FwCH4(tempC = samp$WaterT_C, CH4w.uatm = samp$wCH4_uatm_medhs) # this needs to be double-checked

#### CONVERT umol/m3 to umol/L

samp$wCO2_uM_med <- samp$wCO2_umolm3_med / 1000
samp$wCH4_uM_med <- samp$wCH4_umolm3_med / 1000

#### Final spreadsheet for sharing: Getting averages and removing columns ####

DMV_GHG_med <- samp %>%
  select(Site, Sample_Type, Site_ID, Sample_Date, wCO2_uM_med, wCH4_uM_med) %>%
  group_by(Site_ID) %>%
  summarize(CO2_median_uM = median(wCO2_uM_med, na.rm = TRUE), CH4_median_uM = median(wCH4_uM_med, na.rm = TRUE))
  
# Save updated dataframe, samp 
write.csv(samp, "2021-05/202105_GHG_Wetlands.csv")

# Save clean, averaged spreadsheet
write.csv(DMV_GHG_med, "202105_GHG_Clean.csv")
