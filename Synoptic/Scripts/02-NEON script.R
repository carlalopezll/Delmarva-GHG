# Bretz GHG calculations
# Adapted by Carla López Lloreda for use with Delmarva GHG data
# 11/10/2021

library(ggplot2)
library(dplyr)

# Read synoptic file

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

inputFile <- samp
# inputFile <- read.csv("2020-11/202011_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-02/202102_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-05/202105_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-06/202106_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-09/202109_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-10/202110_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-12/202112_GHG_Wetlands.csv")
# inputFile <- read.csv("2022-03/202203_GHG_Wetlands.csv")
# inputFile <- read.csv("2021-11/202111_GHG_Wetlands.csv")

# Standard pressure and lab temperature

inputFile$baro <- 102
inputFile$headspaceTemp.C <- 20
            
# rename columns
inputFile <- inputFile %>%
  rename(gasVolume = AirV_mL,
         waterVolume = WaterV_mL,
         waterTemp.C = WaterT_C,
         CO2.GC = CO2_ppm,
         CO2air.GC = AirCO2_med_ppm,
         CH4.GC = CH4_ppm,
         CH4air.GC = AirCH4_med_ppm)

def.calc.sdg.conc <- function(
  inputFile,
  volGas = "gasVolume",
  volH2O = "waterVolume",
  baro = "baro",
  waterTemp = "waterTemp.C",
  headspaceTemp = "headspaceTemp.C",
  eqCO2 = "CO2.GC",
  sourceCO2 = "CO2air.GC",
  eqCH4 = "CH4.GC",
  sourceCH4 = "CH4air.GC",
  eqN2O = "concentrationN2OGas",
  sourceN2O = "concentrationN2OAir"
) {
  if(typeof(inputFile) == "character"){
    inputFile <- read.csv(inputFile)
  }
  ##### Constants #####
  cGas<-8.3144598 #universal gas constant (J K-1 mol-1)
  cKelvin <- 273.15 #Conversion factor from Kelvin to Celsius
  cPresConv <- 0.000001 # Constant to convert mixing ratio from umol/mol (ppmv) to mol/mol. Unit conversions from kPa to Pa, m^3 to L, cancel out.
  cT0 <- 298.15#Henry's law constant T0
  #Henry's law constants and temperature dependence from Sander (2015) DOI: 10.5194/acp-15-4399-2015
  ckHCO2 <- 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
  ckHCH4 <- 0.000014 #mol m-3 Pa, range: 0.0000096 - 0.000092
  ckHN2O <- 0.00024 #mol m-3 Pa, range: 0.00018 - 0.00025
  cdHdTCO2 <- 2400 #K, range: 2300 - 2600
  cdHdTCH4 <- 1900 #K, range: 1400-2400
  cdHdTN2O <- 2700 #K, range: 2600 - 3600
  ##### Populate mean global values for reference air where it isn't reported #####
  inputFile[,sourceCO2] = ifelse(is.na(inputFile[,sourceCO2]),# if reported as NA
                                 405, # use global mean https://www.esrl.noaa.gov/gmd/ccgg/trends/global.html
                                 inputFile[,sourceCO2])
  inputFile[,sourceCH4] = ifelse(is.na(inputFile[,sourceCH4]), # use global average if not measured
                                 1.85, #https://www.esrl.noaa.gov/gmd/ccgg/trends_ch4/
                                 inputFile[,sourceCH4])
  ##### Calculate dissolved gas concentration in original water sample #####
  inputFile$dissolvedCO2 <- NA
  inputFile$dissolvedCO2 <- inputFile[,baro] * cPresConv *
    (inputFile[,volGas]*(inputFile[,eqCO2] - inputFile[,sourceCO2])/(cGas * (inputFile[,headspaceTemp] + cKelvin) * inputFile[,volH2O]) +
       ckHCO2 * exp(cdHdTCO2*(1/(inputFile[,headspaceTemp] + cKelvin) - 1/cT0))* inputFile[,eqCO2])
  inputFile$dissolvedCH4 <- NA
  inputFile$dissolvedCH4 <- inputFile[,baro] * cPresConv *
    (inputFile[,volGas]*(inputFile[,eqCH4] - inputFile[,sourceCH4])/(cGas * (inputFile[,headspaceTemp] + cKelvin) * inputFile[,volH2O]) +
       ckHCH4 * exp(cdHdTCH4*(1/(inputFile[,headspaceTemp] + cKelvin) - 1/cT0))* inputFile[,eqCH4])
  #Round to significant figures
  inputFile$dissolvedCO2.molL <- signif(inputFile$dissolvedCO2, digits = 3)
  inputFile$dissolvedCH4.molL <- signif(inputFile$dissolvedCH4, digits = 3)
  return(inputFile)
}

# Get output
gases <- def.calc.sdg.conc(inputFile)

# dissolved units in their output are mol L-1

# for umol
#convert *1e06 mol to umol and *1000 l to m3

gases$dCO2.umol <- gases$dissolvedCO2 * 1000000
gases$dCH4.umol <- gases$dissolvedCH4 * 1000000

#for mmol ***I usually start with this one
#mmol *1000 mol to mmol *1000 l to m3
# gases$dCO2.mmol <- gases$dissolvedCO2.molL * 1000000
# gases$dCH4.mmol <- gases$dissolvedCH4.molL * 1000000

# Compare Hotchkiss script to NEON script

ggplot(gases, aes(x= dCH4.umol, y = wCH4_uM_med)) +
  geom_point()

ggplot(gases, aes(x= dCO2.umol, y = wCO2_uM_med)) +
  geom_point()

# mg/L
# pvy20_neon$dCO2.mgl <- pvy20_neon$dissolvedCO2 * 44.01 * 1000
# check if output makes sense

# Write csv

write.csv(gases, paste0(date, "_GHG_NEON.csv"), row.names=FALSE)
