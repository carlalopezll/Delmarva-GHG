# Script to calculate daily and detrended metrics from Delmarva sensors
# Carla López Lloreda

# load libraries
library(ggplot2)
library(readr)

# read in high-frequency data
sensors_hf <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# extracting a date column
sensors_hf <- sensors_hf %>%
  mutate(date = as.Date(Timestamp_corrected))

# need to finish figuring out how to add the precip sum correctly 

# sensors_daily2 <- sensors_hf %>%
#   mutate(date_day = as.Date(date)) %>%
#   group_by(Site_ID, date_day) %>%
#   mutate(
#     daily_precip = sum(precip_mm, na.rm = TRUE),
#   )

# calculating daily metrics
sensors_daily <- sensors_hf %>%
  group_by(Site_ID, date) %>%
  summarise(CO2_uatm_mean = mean(CO2_cal_uatm, na.rm = TRUE),
            CO2_uatm_sd = sd(CO2_cal_uatm, na.rm = TRUE),
            CO2_uatm_amplitude = max(CO2_cal_uatm, na.rm = TRUE) - min(CO2_cal_uatm, na.rm = TRUE),
            CO2_CV = 100*(sd(CO2_cal_uatm, na.rm = TRUE) / mean(CO2_cal_uatm, na.rm = TRUE)),
            wl_mean = mean(waterLevel, na.rm = TRUE),
            wl_CV = 100*(sd(waterLevel, na.rm = TRUE) / mean(waterLevel, na.rm = TRUE)),
            temp_mean = mean(i.Logger_TempC, na.rm = TRUE),
            ) %>%    
  ungroup()




# Add a column to indicate if water level is increasing or decreasing
sensors_daily <- sensors_daily %>%
  arrange(Site_ID, date) %>%
  group_by(Site_ID) %>%
  mutate(delta5_wl = wl_mean - lag(wl_mean, 5),
         waterLevel_trend = case_when(
           delta5_wl > 0 ~ "Increasing",
           delta5_wl < 0 ~ "Decreasing",
           TRUE ~ NA_character_
         )) %>%
  ungroup()

write_csv(sensors_daily, "CO2/data/processed data/sensors_daily.csv")