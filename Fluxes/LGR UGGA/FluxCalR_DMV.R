## Script to  FluxCalR for calculating fluxes from UGGA data
## A Hounshell, 25 Jan 2021
## Updated by C López Lloreda for Delmarva wetlands projects
## Last updated 3/12/2024

# Script following: https://github.com/junbinzhao/FluxCalR

# Install FluxCalR (if not already installed! If installed, skip to library(FluxCalR))

# install.packages("remotes")
# remotes::install_github("junbinzhao/FluxCalR",build_vignettes = TRUE)

library(FluxCalR)
library(tidyverse)
library(lubridate)

# Load in data

wd <- setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Fluxes/LGR UGGA/Data")

# You'll want to save this script in the same working directory to keep a record of what files you have corrected.

# Load in flux data - load in all text files from the 'TextFiles' folder here.
# This can be used as a record of what files you have already corrected.

# Could probably be more efficient with this read in 
# Check James's scripts

flux_lgr_1 <- LoadLGR(file ="./gga_2001-12-31_f0045.txt",
                      time_format = "mdy_HMS")

flux_lgr_2 <- LoadLGR(file ="./gga_2001-12-31_f0046.txt",
                      time_format = "mdy_HMS")

flux_lgr_3 <- LoadLGR(file ="./gga_2001-12-31_f0047.txt",
                      time_format = "mdy_HMS")

flux_lgr_4 <- LoadLGR(file ="./gga_2001-12-31_f0048.txt",
                      time_format = "mdy_HMS")

flux_lgr_5 <- LoadLGR(file ="./gga_2001-12-31_f0049.txt",
                      time_format = "mdy_HMS")

# Select times when the UGGA was on/off the water
# Select the time point BEFORE the peak for both reps
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for the second peak. 
# When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak saved as time_cue_x

# Still need to fix the actual dates and sites on each of these

time_cue_2 <- SelCue(flux_lgr_2, flux="CO2", cue="End", save=F)%>%
  mutate(Site = c("TS"), Date_real = as.Date("2022-06-20"))

time_cue_3 <- SelCue(flux_lgr_3,flux="CO2",cue="End", save=F)%>%
  mutate(Site = c("TS"), Date_real = as.Date("2022-06-20"))

time_cue_4 <- SelCue(flux_lgr_4,flux="CO2",cue="End",save=F)%>%
  mutate(Site = c("TS"), Date_real = as.Date("2022-06-13"))

time_cue_5 <- SelCue(flux_lgr_5,flux="CH4",cue="End",save=F)%>%
  mutate(Site = c("TS"), Date_real = as.Date("2022-06-13"))

time_cue_6 <- SelCue(flux_lgr_6,flux="CH4",cue="End",save=F)%>%
  mutate(Site = c("TS"), Date_real = as.Date("2022-06-13"))


# Then calculate fluxes
# We use 10 minute windows
# Volume and area stay the same (same chamber as the Carey lab is used)
# Need to change other columns that specific site details

Flux_output2 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

Flux_output3 <- FluxCal(data = flux_lgr_3, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_3, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

Flux_output4 <- FluxCal(data = flux_lgr_4, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_4, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

Flux_output5 <- FluxCal(data = flux_lgr_5, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_5, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

Flux_output6 <- FluxCal(data = flux_lgr_6, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_6, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

# Combine all flux outputs
flux_output <- rbind(Flux_output3,
                     Flux_output4
                     )

detach('package:dplyr')
library(dplyr)

# Get together for publication to EDI
flux_co2 <- flux_output %>% 
  filter(Gas == "CO2") %>% 
  rename(co2_slope_ppmS = Slope, co2_R2 = R2, co2_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_ch4 <- flux_output %>% 
  filter(Gas == "CH4") %>% 
  rename(ch4_slope_ppmS = Slope, ch4_R2 = R2, ch4_flux_umolCm2s = Flux) %>% 
  select(-Gas) %>%
  mutate(ch4_flux_nmolCm2s = ch4_flux_umolCm2s*1000)

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num","Date","Start","End","Ta","Reservoir","Site","Date_real"))

flux_all <- flux_all %>% 
  rename(Rep = Num, Temp_C = Ta) %>% 
  mutate (co2_flux_umolCm2s_flag = 0, ch4_flux_umolCm2s_flag = 0)

# Filter out R2<0.90

# Plot fluxes
flux_all%>%
  ggplot(aes(x = as.Date(Date_real), y=ch4_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CH4 flux (µumol/m2/s)")

flux_all%>%
  ggplot(aes(x = as.Date(Date_real), y=co2_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CO2 flux (µumol/m2/s)")

flux_all%>%
  ggplot(aes(x = as.Date(Date_real),y=ch4_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CH4 flux (µumol/m2/s)")+
  ggtitle("BVR Site 01")

flux_all_2%>%
  filter(Site==1)%>%
  ggplot(aes(x = as.Date(Date_real),y=co2_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CO2 flux (µumol/m2/s)")+
  ggtitle("BVR Site 01")

flux_all_2%>%
  filter(Site==50)%>%
  ggplot(aes(x = as.Date(Date_real),y=ch4_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CH4 flux (µumol/m2/s)")+
  xlab("Date")+
  ggtitle("BVR Site 50")

flux_all_2%>%
  filter(Site==50)%>%
  ggplot(aes(x = as.Date(Date_real),y=co2_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  geom_vline(xintercept = as.Date("2022-05-29"))+
  ylab("CO2 flux (µumol/m2/s)")+
  xlab("Date")+
  ggtitle("BVR Site 50")

flux_all_2 = flux_all_2%>%
  mutate(Date=Date_real)%>%
  select(-Date_real)

# Export out fluxes
# write_csv(flux_all_2,"./2022_season_Flux_Output.csv")
