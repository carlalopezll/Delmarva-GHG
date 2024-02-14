## Script to *try* FluxCalR for calculating fluxes from UGGA data
## A Hounshell, 25 Jan 2021

## Updated by C López Lloreda for Delmarva wetlands projects

# Script following: https://github.com/junbinzhao/FluxCalR

# Install FluxCalR (if not already installed! If installed, skip to library(FluxCalR))

install.packages("remotes")
remotes::install_github("junbinzhao/FluxCalR",build_vignettes = TRUE)

library(FluxCalR)
library(tidyverse)
library(lubridate)

# Load in data

wd <- setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/LGR UGGA/Data")

# You'll want to save this script in the same working directory to keep a record of what files you have corrected.

# Load in flux data - load in all text files from the 'TextFiles' folder here.
# This can be used as a record of what files you have already corrected.

# Could probably be more efficient with this read in 
# Check James's scripts

flux_lgr_3 <- LoadLGR(file ="./gga_2001-12-31_f0071.txt",
                      time_format = "mdy_HMS")

flux_lgr_4 <- LoadLGR(file ="./gga_2001-12-31_f0046.txt",
                      time_format = "mdy_HMS")

flux_lgr_5 <- LoadLGR(file ="./gga_2001-12-31_f0047.txt",
                      time_format = "mdy_HMS")

# flux_lgr_2 <- LoadLGR(file ="./gga_2001-12-31_f0032.txt",
#                       time_format = "mdy_HMS")%>%
#   filter(as_datetime(Time)>as_datetime("2022-06-20 12:04:02"))
# 
# flux_lgr_6 <- LoadLGR(file ="./gga_2001-12-31_f0024.txt",
#                       time_format = "mdy_HMS")%>%
#   filter(year(Time)>2020)
# flux_lgr_7 <- LoadLGR(file ="./gga_2001-12-31_f0023.txt",
#                       time_format = "mdy_HMS")%>%
#   filter(as_datetime(Time)>as_datetime("2022-06-13 11:13:02"))
# flux_lgr_8 <- LoadLGR(file ="./gga_2001-12-31_f0020.txt",
#                       time_format = "mdy_HMS")
# flux_lgr_9 <- LoadLGR(file ="./gga_2001-12-31_f0019.txt",
#                       time_format = "mdy_HMS")
# flux_lgr_10 <- LoadLGR(file ="./gga_2001-12-31_f0018.txt",
#                       time_format = "mdy_HMS")
# flux_lgr_11 <- LoadLGR(file ="./gga_2001-12-31_f0017.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_12 <- LoadLGR(file ="./gga_2001-12-31_f0016.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_13 <- LoadLGR(file ="./gga_2001-12-31_f0015.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_14 <- LoadLGR(file ="./gga_2001-12-31_f0013.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_15 <- LoadLGR(file ="./gga_2001-12-31_f0011.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_16 <- LoadLGR(file ="./gga_2001-12-31_f0034.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_17 <- LoadLGR(file ="./gga_2001-12-31_f0035.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_18 <- LoadLGR(file ="./gga_2001-12-31_f0036.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_19 <- LoadLGR(file ="./gga_2001-12-31_f0037.txt",
#                        time_format = "mdy_HMS")
# flux_lgr_20 <- LoadLGR(file ="./gga_2001-12-31_f0038.txt",
#                        time_format = "mdy_HMS")
# #issue with gga_2001-12-31_f0038.txt --> ch4 went up to 40
# flux_lgr_22 <- LoadLGR(file ="./gga_2001-12-31_f0040.txt",
#                        time_format = "mdy_HMS")

# Select times when the UGGA was on/off the water
# Select the time point BEFORE the peak for both reps
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for the second peak. 
# When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak saved as time_cue_x

time_cue_2 <- SelCue(flux_lgr_2,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-06-20"))

time_cue_3 <- SelCue(flux_lgr_3,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-20"))
time_cue_4 <- SelCue(flux_lgr_4,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-06-13"))
time_cue_5 <- SelCue(flux_lgr_5,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-13"))
time_cue_6 <- SelCue(flux_lgr_6,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-13"))
time_cue_7 <- SelCue(flux_lgr_7,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-13"))
time_cue_8 <- SelCue(flux_lgr_8,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-06-07"))
time_cue_9 <- SelCue(flux_lgr_9,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-07"))
time_cue_10 <- SelCue(flux_lgr_10,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-05-31"))
time_cue_11 <- SelCue(flux_lgr_11,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-05-31"))
time_cue_12 <- SelCue(flux_lgr_12,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-05-17"))
time_cue_13 <- SelCue(flux_lgr_13,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-05-17"))
time_cue_14 <- SelCue(flux_lgr_14,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-05-09"))
time_cue_15 <- SelCue(flux_lgr_15,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-05-09"))
time_cue_16 <- SelCue(flux_lgr_16,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-05-05"))
time_cue_17 <- SelCue(flux_lgr_17,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-05-05"))
time_cue_18 <- SelCue(flux_lgr_18,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-06-27"))
time_cue_19 <- SelCue(flux_lgr_19,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-06-27"))
time_cue_20 <- SelCue(flux_lgr_20,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(1), Date_real = as.Date("2022-07-05"))
time_cue_22 <- SelCue(flux_lgr_22,flux="CH4",cue="End",save=F)%>%
  mutate(Reservoir = c("BVR"), Site = c(50), Date_real = as.Date("2022-07-05"))

# Then calculate fluxes
# We use 10 minute windows
# Volume and area stay the same (same chamber is used)
# Need to change other columns that specific site details

Flux_output2 <- FluxCal(data = flux_lgr_2, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_2, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output3 <- FluxCal(data = flux_lgr_3, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_3, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output4 <- FluxCal(data = flux_lgr_4, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_4, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output5 <- FluxCal(data = flux_lgr_5, # Dataframe loaded in
                        win = 10, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_5, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output6 <- FluxCal(data = flux_lgr_6, # Dataframe loaded in
                        win = 2, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_6, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output7 <- FluxCal(data = flux_lgr_7, # Dataframe loaded in
                        win = 3, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_7, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output8 <- FluxCal(data = flux_lgr_8, # Dataframe loaded in
                        win = 2, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_8, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output9 <- FluxCal(data = flux_lgr_9, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_9, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output10 <- FluxCal(data = flux_lgr_10, # Dataframe loaded in
                        win = 4, # Window length = 4 minutes
                        vol = 0.020876028*1000, # Volume of trap in liters
                        area = 0.1451465, # Area of trap in m^2
                        df_cue = time_cue_10, # End times selected using SelCue
                        cue_type = "End", # Designate that these times are for the end
                        ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                        other = c("Reservoir","Site","Date_real"),
                        output = FALSE)

Flux_output11 <- FluxCal(data = flux_lgr_11, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_11, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output12 <- FluxCal(data = flux_lgr_12, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_12, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output13 <- FluxCal(data = flux_lgr_13, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_13, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output14 <- FluxCal(data = flux_lgr_14, # Dataframe loaded in
                         win = 3, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_14, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output15 <- FluxCal(data = flux_lgr_15, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_15, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output16 <- FluxCal(data = flux_lgr_16, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_16, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output17 <- FluxCal(data = flux_lgr_17, # Dataframe loaded in
                         win = 3, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_17, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output18 <- FluxCal(data = flux_lgr_18, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_18, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output19 <- FluxCal(data = flux_lgr_19, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_19, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output20 <- FluxCal(data = flux_lgr_20, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_20, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

Flux_output22 <- FluxCal(data = flux_lgr_22, # Dataframe loaded in
                         win = 4, # Window length = 4 minutes
                         vol = 0.020876028*1000, # Volume of trap in liters
                         area = 0.1451465, # Area of trap in m^2
                         df_cue = time_cue_22, # End times selected using SelCue
                         cue_type = "End", # Designate that these times are for the end
                         ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                         other = c("Reservoir","Site","Date_real"),
                         output = FALSE)

# Combine all flux outputs
flux_output <- rbind(Flux_output2,
                     Flux_output3,
                     Flux_output4,
                     Flux_output5,
                     Flux_output6,
                     Flux_output7,
                     Flux_output8,
                     Flux_output9,
                     Flux_output10,
                     Flux_output11,
                     Flux_output12,
                     Flux_output13,
                     Flux_output14,
                     Flux_output15,
                     Flux_output16,
                     Flux_output17,
                     Flux_output18,
                     Flux_output19,
                     Flux_output20,
                     Flux_output22
                     )

# Get together for publication to EDI
flux_co2 <- Flux_output5 %>% 
  filter(Gas == "CO2") %>% 
  rename(co2_slope_ppmS = Slope, co2_R2 = R2, co2_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_ch4 <- Flux_output5 %>% 
  filter(Gas == "CH4") %>% 
  rename(ch4_slope_ppmS = Slope, ch4_R2 = R2, ch4_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num","Date","Start","End","Ta","Reservoir","Site","Date_real"))

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
