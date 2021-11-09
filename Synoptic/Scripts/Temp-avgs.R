# Looking at plots across all sites

# From the merged file?

air_summary <- GHG %>%
  filter(Rep == "Air") %>%
  group_by(Air_Location) %>%
  summarize(AirCO2_min_ppm = min(CO2_ppm, na.rm = TRUE), AirCO2_med_ppm = median(CO2_ppm, na.rm = TRUE), 
            AirCO2_max_ppm = max(CO2_ppm, na.rm = TRUE), AirCH4_min_ppm = min(CH4_ppm, na.rm = TRUE),
            AirCH4_med_ppm = median(CH4_ppm, na.rm = TRUE), AirCH4_max_ppm = max(CH4_ppm, na.rm = TRUE))