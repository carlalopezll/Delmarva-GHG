#### Averages, ranges, and std per site ####

source("CO2/scripts/0-setup.R")

color_palette <- c("DK" = "#F8766D", 
                   "ND" = "#00BA38", 
                   "TS" = "#619CFF")

sensor_summary <- co2 %>%
  group_by(Site_ID) %>%
  summarize(CO2_sensor_avg_ppm = mean(CO2_ppm_corr),
            CO2_sensor_min_ppm = min(CO2_ppm_corr),
            CO2_sensor_max_ppm = max(CO2_ppm_corr),
            CO2_sensor_corr_avg_uatm = mean(CO2_cal_uatm),
            CO2_sensor_corr_min_uatm = min(CO2_cal_uatm),
            CO2_sensor_corr_max_uatm = max(CO2_cal_uatm))

ggplot(co2, aes(CO2_cal_uatm, fill = Site_ID, color = Site_ID)) +
  geom_density(alpha = 0.1)
