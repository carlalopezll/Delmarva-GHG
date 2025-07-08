# Script for calculating O2-CO2 departures

# Read in merged script

sensors <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# Constants
MOLAR_MASS_CO2 <- 44.01 # g/mol (molecular weight of CO2)
SATURATION_CONCENTRATION <- 1.3 # mmol/L, equilibrium solubility of CO2 at 25°C
SATURATION_CONCENTRATION_MOLAR <- SATURATION_CONCENTRATION / 1000 # Convert to mol/L (1 mmol/L = 0.001 mol/L)

# Function to convert ppm to molarity (mol/L)
convert_ppm_to_molarity <- function(ppm) {
  molarity <- ppm / (MOLAR_MASS_CO2 * 1000) # ppm is mg/L, molecular weight is in g/mol, and we need mol/L
  return(molarity)
}

# Calculate CO2 saturation for each site and timestamp
sensors <- sensors %>%
  mutate(
    # Convert CO2_ppm_corr to molarity (mol/L)
    co2_molarity = convert_ppm_to_molarity(CO2_ppm_corr),
    
    # Calculate saturation level: measured concentration / saturation concentration
    saturation_level = co2_molarity / SATURATION_CONCENTRATION_MOLAR,
    
    DO_conc_uM = DO_conc_mgL * 1000 / 31.999,
    
    DO_sat = DO_perc * 1000 / 31.999
  )

sensors2$DO_Concentration_uM <- sensors2$DO_conc_mgL * 1000 / 31.999
sensors2$DO_sat <- sensors2$DO_perc * 1000 / 31.999

# Need to convert CO2 from ppm to uM
sensors2$CO2_Conc_uM <- sensors2$CO2_ppm_corr * 1000 / 44

# Calculate CO2 saturation
# Henry's law constants and temperature dependence from Sander (2015)

ckHCO2 = 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
cdHdTCO2 = 2400 #K, range: 2300 - 2600
cT0 = 298.15 #Henry's law constant T0

# allData$CO2.sat = (ckHCO2 * exp(cdHdTCO2*(1/(allData$NEON.temp + 273.15) - 1/cT0))) * allData$co2.atm.ppmv * allData$barPres

sensors$CO2_sat <- (ckHCO2 * exp(cdHdTCO2*(1/(sensors$Logger_TempC + 273.15) - 1/cT0))) * 420 * (sensors$Site_AbsPressure.kPa*10)

mean(sensors$Site_AbsPressure.kPa, na.rm = T)*10
# 1109.355

sensors2$CO2_sat <- (ckHCO2 * exp(cdHdTCO2*(1/(15 + 273.15) - 1/cT0))) * 420 * 1109.35



ggplot(sensors, aes(x=saturation_level, y = CO2_sat)) +
  geom_point()

ggplot(sensors, aes(x=DO_perc, y = DO_sat)) +
  geom_point()

# #calculate departures:
# allData$O2.departure<-allData$dissolvedOxygen - allData$DO.sat
# allData$CO2.departure<-allData$CO2.uM.corr - allData$CO2.sat
# #change to if-else if not all sites are using corr data
# allData$CO2.departure.raw<-allData$CO2.uM - allData$CO2.sat
# allData$site=as.factor(allData$site)


# I don't think I'm doing the departure calculations right
sensors2$O2_departure <- sensors2$DO_Concentration_uM / sensors2$DO_sat
sensors2$CO2_departure <- sensors2$CO2_Conc_uM / sensors2$CO2_sat

# calculating departure metrics

sensors3 <- sensors3 %>%
  mutate(offset = abs(sensors3$saturation_level + sensors3$DO_perc) / sqrt(2))

sensors3$DO_CO2 <- sensors3$DO_Concentration_uM/sensors3$CO2_Conc_uM

sensors3$euclidean_distance <- sqrt(sensors3$DO_perc^2 + sensors3$saturation_level^2)


###############################

# calculate the daily slope

sensors3 <- sensors3 %>%
  group_by(Site_ID, date) %>%
  mutate(daily_slope = summary(lm(DO_perc~saturation_level))$coefficients[2,1],
         mean_temp = mean(GP_TempC),
         min_O2 = min(DO_conc_mgL),
         mean_wl = mean(waterLevel))

# how many datapoints do we have?
count <- n(sensors3$date)

# how many days do we have per site?


ggplot(sensors3, aes(x=Timestamp_corrected, y = daily_slope)) +
  geom_point() +
  facet_wrap(~Site_ID, ncol = 3) +
  ylim(-2,0) + 
  geom_smooth()

ggplot(sensors3, aes(x=mean_wl, y = daily_slope)) +
  geom_point() +
  facet_wrap(~Site_ID, ncol = 3) +
  ylim(-1,0) + 
  geom_smooth(method = "lm")

str(sensors3)
ggplot(sensors3, aes(x=min_O2, y = daily_slope)) +
  geom_point() +
  facet_wrap(~Site_ID, ncol = 3) +
  ylim(-1,0) +
  geom_smooth(method = "lm")
