# Correcting eosGP sensor data with grab samples
# Created by Carla Lopez Lloreda for Delmarva project
# Last updated 4/10/2023

# Load libraries
library(plyr)
library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)
library(ggplot2)
library(lubridate)

# Get pressure-corrected sensor data
sensors <- read_csv("CO2/data/processed data/eosGP CO2_pressure corrected.csv")

# Get grab calibration checks
cal_checks <- na.omit(read_csv("CO2/eos GP CO2 rating curve.csv"))

# Fixing date-time on cal check spreadsheet

cal_checks$Timestamp_corrected <- as.POSIXct(paste(cal_checks$Date, cal_checks$Sampling_time),
                                             format = "%m/%d/%Y %H:%M")

ggplot(cal_checks, aes(x=wCO2_uatm_avg, y = CO2_eosGP_ppm, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm")

# Can I do something to extract the equations?


# Create new column that corrects the sensor data with equations from site-specific CO2 rating curves

# DK equation
# y = 0.9209x - 3693.5
# R^2 = 0.9996

# ND equation
# y = 0.6395x - 1506.5
# R^2 = 0.8765

# TS equation
# y = 0.6192x - 1084.9
# R^2 = 0.7948

sensors <- mutate(sensors, CO2_cal_uatm = 
                    ifelse(Site_ID == "DK", 0.9209 * sensors$CO2_ppm_corr - 3693.5, 
                           ifelse(Site_ID == "ND", 0.6395 * sensors$CO2_ppm_corr - 1506.5,
                                  ifelse(Site_ID == "TS", 0.6192 * sensors$CO2_ppm_corr - 1084.9, NA))))

# Plot time-series of sensor data without ANY corrections

ggplot(sensors, aes(x= Timestamp, y = CO2_HiConc_ppm, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "4 months") +
  labs(x= "", y = "Raw CO2 (ppm)") +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        legend.position = c(.85, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

# ggsave("Graphs/CO2_ppm_raw.jpg")

# Plot time-series of pressure-corrected sensor data without calibration corrections

ggplot(sensors, aes(x= Timestamp, y = CO2_ppm_corr, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "4 months") +
  labs(x= "", y = "Pressure-corrected CO2 (ppm)") +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        legend.position = c(.80, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

# ggsave("Graphs/CO2_ppm_press corrected.jpg")

# Plot time-series of grab samples
  
ggplot(cal_checks, aes(x= Timestamp_corrected, y = wCO2_uatm_avg, color = Site_ID)) +
  geom_point() +
  theme_bw() +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=18),
          legend.title=element_text(size=18),
          legend.position = c(.80, .95),
          legend.justification = c("center", "top"),
          legend.box.just = "center",
          legend.margin = margin(6, 6, 6, 6))

# Plot time-series of grab-corrected sensor data

ggplot(sensors, aes(x= Timestamp, y = CO2_cal_uatm, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months") +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        legend.position = c(.45, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("Graphs/CO2_corrected_uatm_w outliers.jpg")

# Save csv

write_csv(sensors, "CO2/data/processed data/eosGP_JL.csv")

# Plot time-series of un-corrected sensor data along with grab samples for EACH SITE

# Dark Bay (DK)
ggplot(sensors_clean[sensors_clean$Site %in% "DK", ], 
       aes(Timestamp, CO2_ppm_corr, color = Site)) +
  geom_line(linetype = "twodash") +
  geom_point(data = cal_checks[cal_checks$Site %in% "DK", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "", y = "CO2 uncorrected (ppm)")

ggsave("Graphs/Uncorrected CO2 w grab_DK.jpg")

# North Dog Bone (ND)
ggplot(sensors_clean[sensors_clean$Site %in% "ND", ], 
       aes(Timestamp, CO2_ppm_corr, color = Site)) +
  geom_line(linetype = "twodash", color = "#00BA38") +
  geom_point(data = cal_checks[cal_checks$Site %in% "ND", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             fill = "#00BA38", shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "", y = "CO2 uncorrected (ppm)")

ggsave("Graphs/Uncorrected CO2 w grab_ND.jpg")

# Tree Stand (TS)
ggplot(sensors_clean[sensors_clean$Site %in% "TS", ], 
       aes(Timestamp, CO2_ppm_corr, color = Site)) +
  geom_line(linetype = "twodash", color = "#619CFF") +
  geom_point(data = cal_checks[cal_checks$Site %in% "TS", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             fill = "#619CFF", shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  labs(x= "", y = "CO2 uncorrected (ppm)")

ggsave("Graphs/Uncorrected CO2 w grab_TS.jpg")


# Plot corrected sensor data with grab samples for EACH SITE

# Dark Bay (DK)
ggplot(sensors_clean[sensors_clean$Site %in% "DK", ], 
       aes(Timestamp, CO2_cal_uatm, color = Site)) +
  geom_line() +
  geom_point(data = cal_checks[cal_checks$Site %in% "DK", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "", y = "CO2 corrected (uatm)")

ggsave("Graphs/Corrected CO2 w grab_DK.jpg")

# North Dog Bone (ND)
ggplot(sensors_clean[sensors_clean$Site %in% "ND", ], 
       aes(Timestamp, CO2_cal_uatm, color = Site)) +
  geom_line(color = "#00BA38") +
  geom_point(data = cal_checks[cal_checks$Site %in% "ND", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             fill = "#00BA38", shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "", y = "CO2 corrected (uatm)")

ggsave("Graphs/Corrected CO2 w grab_ND.jpg")

# Tree Stand (TS)
ggplot(sensors_clean[sensors_clean$Site %in% "TS", ], 
       aes(Timestamp, CO2_cal_uatm, color = Site)) +
  geom_line(color = "#619CFF") +
  geom_point(data = cal_checks[cal_checks$Site %in% "TS", ],
             aes(Timestamp_corrected, wCO2_uatm_avg, fill = Site, size = 20),
             fill = "#619CFF", shape= 23, color= "darkred", size= 5) +
  ylim(0, 20000) +
  scale_x_datetime(breaks = "1 month") +
  theme_bw() +
  labs(x= "", y = "CO2 corrected (ppm)")

ggsave("Graphs/Corrected CO2 w grab_TS.jpg")


# Plot battery

ggplot(sensors, aes(x= Timestamp, y = BattV, color = Site)) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "1 months") +
  labs(x= "", y = "Battery (V)") +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        legend.position = c(.85, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("JL battery.jpg")
