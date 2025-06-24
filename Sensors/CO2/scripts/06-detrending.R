# Detrending time series

# Load libraries

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr) # reading and writing csvs
library(udunits2) # unit conversions
library(lubridate)
library(scales)
library(methods)
library(ggplot2)
library(plotly)
library(lubridate)
library(astsa) # for detrending
library(zoo) # another one for detrending

# Read in corrected and cleaned eos GP CO2 data
sensors <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# Theme
theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

CO2_lab <- expression(paste("C","O"[2]^{}*" (uatm)"))

# Calculate daily CO2 mean

sensors <- sensors %>%
  mutate(date = as.Date(Timestamp)) %>%  # Extract date from timestamp
  group_by(Site_ID, date) %>%               # Group by Site and Date
  mutate(daily_mean_CO2_cal_uatm = mean(CO2_cal_uatm, na.rm = TRUE)) %>%
  ungroup()

# Calculate daily CO2 variability

sensors <- sensors %>%
  mutate(date = as.Date(Timestamp)) %>%  # Extract date from timestamp
  group_by(Site_ID, date) %>%               # Group by Site and Date
  mutate(daily_sd_CO2_cal_uatm = sd(CO2_cal_uatm, na.rm = TRUE)) %>%
  ungroup()

# Calculate daily amplitude

sensors <- sensors %>%
  mutate(date = as.Date(Timestamp)) %>%  # Extract date from timestamp
  group_by(Site_ID, date) %>%               # Group by Site and Date
  mutate(daily_amp_CO2_cal_uatm = max(CO2_cal_uatm, na.rm = TRUE) - mean(CO2_cal_uatm, na.rm = TRUE)) %>%
  ungroup()

ggplot(sensors, aes(x= daily_sd_CO2_cal_uatm, y = daily_amp_CO2_cal_uatm, color = log10(precip_mm))) +
  geom_point()

summary(lm(daily_amp_CO2_cal_uatm~daily_sd_CO2_cal_uatm, data = sensors))

g <- ggplot(sensors, aes(Timestamp, y = daily_mean_CO2_cal_uatm, color = Site_ID)) +
  geom_point() +
  geom_smooth()

g
ggplotly(g)

# Calculate daily mean water level

sensors <- sensors %>%
  mutate(date = as.Date(Timestamp)) %>%  # Extract date from timestamp
  group_by(Site_ID, date) %>%               # Group by Site and Date
  mutate(daily_mean_wl = mean(waterLevel, na.rm = TRUE)) %>%
  ungroup()  

ggplot(sensors, aes(x= daily_mean_wl, y = daily_mean_CO2_cal_uatm, color = season)) +
  geom_point() +
  theme_bw() +
  labs(x= "Water level (m)", y = CO2_lab) +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position = "none",
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  ylim(0, 25000) +
  facet_wrap(~Site_Name, scales = "free") +
  geom_smooth(method = "lm", color = "black") +
  theme

ggsave("CO2/graphs/CO2-WL by season.jpg", width = 18, height = 8, units = "in")

# Other option

# Detrending

detrended_sensors <- as.zoo(ND$CO2_cal_uatm)
detrended_sensors <- ND[,13:12]
head(detrended_sensors)

plot(detrended_sensors)

diff.ts <- diff(detrended_sensors$CO2_cal_uatm)
plot(diff.ts)



detrended_data <- sensors %>%
  group_by(Site_ID) %>%  # Group by Site_ID if needed
  mutate(detrended_CO2 = CO2_HiConc_ppm - predict(lm(CO2_HiConc_ppm ~ Timestamp, data = .))) %>%
  ungroup()


window_size <- 3

# Load necessary libraries
library(seewave)      # for Hilbert transform
library(wsyn)         # for cleandat()
library(dplyr)        # for data manipulation
library(zoo)          # for interpolation
library(lubridate)    # for date handling

# Load your data (replace with your actual data frame)
# data <- read.csv("your_data.csv")

# Example data frame structure
# Assume your data has columns: Timestamp, Site_ID, temperature, light, DOsat
sensors2$Timestamp <- as.POSIXct(sensors2$Timestamp)

co2$numeric_times <- as.numeric(co2$Timestamp)

# 1. Preprocess data: detrending and z-scoring
cleaned_data <- cleandat(co2$CO2_HiConc_ppm, co2$numeric_times, clev= 2)

# 2. Interpolate missing data (max gap of 6 hours)
interpolated_data <- cleaned_data %>%
  group_by(Site_ID) %>%
  mutate(across(everything(), ~ na.approx(.x, maxgap = 6*60))) %>%  # assuming data is in minutes
  ungroup()

# 3. Aggregate data into weekly time steps
weekly_data <- interpolated_data %>%
  mutate(Week = floor_date(Timestamp, "week")) %>%
  group_by(Week, Site_ID) %>%
  summarize(
    temperature = mean(temperature, na.rm = TRUE),
    light = mean(light, na.rm = TRUE),
    DOsat = mean(DOsat, na.rm = TRUE),
    .groups = 'drop'
  )

# 4. Filter out weeks with any missing data
weekly_data <- weekly_data %>%
  filter(complete.cases(.))

# 5. Calculate synchrony using Hilbert transform
# This assumes you have a function or method to calculate synchrony
synchrony_results <- list()
site_ids <- unique(weekly_data$Site_ID)

for (site in site_ids) {
  site_data <- weekly_data %>% filter(Site_ID == site)
  
  # Perform Hilbert transform (adjust according to your needs)
  hilbert_transform <- ifreq(site_data$temperature)  # Example for temperature
  
  # Store results
  synchrony_results[[site]] <- hilbert_transform
}

# 6. Summary of results
# This part will depend on how you wish to summarize or visualize your synchrony analysis
summary_results <- data.frame(
  Site_ID = site_ids,
  Synchrony_Mean = sapply(synchrony_results, mean),
  Synchrony_SD = sapply(synchrony_results, sd)
)

# View summary
print(summary_results)



