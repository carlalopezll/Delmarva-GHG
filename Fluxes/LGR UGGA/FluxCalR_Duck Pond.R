## Script to *try* FluxCalR for calculating fluxes from UGGA data
## A Hounshell, 25 Jan 2021

## Updated by C López Lloreda for Duck Pond fluxes

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

flux_lgr_1 <- LoadLGR(file ="./gga_2001-12-31_f0012.txt",
                      time_format = "mdy_HMS")

flux_lgr_2 <- LoadLGR(file ="./gga_2001-12-31_f0023.txt",
                      time_format = "mdy_HMS")

flux_lgr_3 <- LoadLGR(file ="./gga_2001-12-31_f0024.txt",
                      time_format = "mdy_HMS")

# Select times when the UGGA was on/off the water
# Select the time point BEFORE the peak for both reps
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for the second peak. 
# When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak saved as time_cue_x

time_cue_1 <- SelCue(flux_lgr_1,flux="CH4",cue="End",save=F)%>%
  mutate(Site = c("Duck Pond"), Date_real = as.Date("2023-04-17"))

time_cue_2 <- SelCue(flux_lgr_2,flux="CH4",cue="End",save=F)%>%
  mutate(Site = c("Duck Pond"), Date_real = as.Date("2023-05-11"))

time_cue_3 <- SelCue(flux_lgr_3,flux="CH4",cue="End",save=F)%>%
  mutate(Site = c("Duck Pond"), Date_real = as.Date("2023-05-11"))

# Then calculate fluxes
# We use 10 minute windows
# Volume and area stay the same (same chamber is used)
# Need to change other columns that specific site details

flux_output1 <- FluxCal(data = flux_lgr_1, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_1, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

flux_output2 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 10, # Window length
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

flux_output3 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 10, # Window length
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Site","Date_real"),
                        output = FALSE)

# Combine all flux outputs
flux_output <- rbind(flux_output2,
                     flux_output3
                     )

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

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num","Date","Start","End","Ta","Site","Date_real"))

flux_all <- flux_all %>% 
  rename(Rep = Num, Temp_C = Ta) %>% 
  mutate (co2_flux_umolCm2s_flag = 0, ch4_flux_umolCm2s_flag = 0)

#col_order <- c("Reservoir","Site","Date","Rep","Start_time","End_time","Temp_C","co2_slope_ppmS","co2_R2",
#               "co2_flux_umolCm2s","ch4_slope_ppmS","ch4_R2","ch4_flux_umolCm2s","co2_flux_umolCm2s_flag",
#               "ch4_flux_umolCm2s_flag")

#flux_all_2 <- flux_all[,col_order]
flux_all_2 <- flux_all

# Plots to double check data!
#flux_all_2$Date <- as.POSIXct(strptime(flux_all_2$Date,"%Y-%m-%d", tz="EST"))

flux_all_2%>%
  ggplot(aes(x = as.Date(Date_real), color = as.factor(Site),y=ch4_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CH4 flux (µumol/m2/s)")

flux_all_2%>%
  ggplot(aes(x = as.Date(Date_real), color = as.factor(Site),y=co2_flux_umolCm2s))+
  geom_smooth()+
  geom_point()+
  ylab("CO2 flux (µumol/m2/s)")

flux_all_2%>%
  filter(Site==1)%>%
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
write_csv(flux_all_2,"./2022_season_Flux_Output.csv")
