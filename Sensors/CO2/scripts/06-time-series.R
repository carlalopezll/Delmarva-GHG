# Plotting eosGP CO2 sensor data + water level
# Created by Carla L?pez Lloreda for Delmarva project
# Last updated 4/8/2023

# Load libraries

library(udunits2) # unit conversions
library(scales)
library(methods)
library(gridExtra)
library(plotly)

# Set up

source("CO2/scripts/0-setup.R")

color_palette <- c("DK" = "#F8766D", 
                   "ND" = "#00BA38", 
                   "TS" = "#619CFF")

sensors <- read_csv("CO2/data/merged sensors_250412.csv")

# Reordering seasons
sensors$season <- factor(sensors$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Reordering site IDs
sensors$Site_ID <- factor(sensors$Site_ID, levels = c("TS", "DK", "ND"))

# Filter so that we use only the time with precip and CO2 data

sensors <- filter(sensors, timestamp < "2022-08-04")


# Rename sites

sensors <- sensors %>%
  mutate(Site_hydroperiod = recode(Site_ID, 
                                   TS = "Short", 
                                   DK = "Medium", 
                                   ND = "Long"))

# Select only the relevant columns to plot
data_relevant <- sensors %>%
  select(timestamp, Site_ID, CO2_cal_uatm, waterLevel, precip_mm, DO_perc)

# Reshape the data to a long format
data_long <- data_relevant %>%
  gather(key = "variable", value = "value", -timestamp, -Site_ID)

# Plot the stacked time-series graph
ggplot(data_long, aes(x = timestamp, y = value, color = Site_ID)) +
  geom_point(size = 0.6) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +  # Separate panels for each variable
  labs(title = "Stacked Time-Series Plot by Variable", x = "Date", y = "Values") +
  theme_minimal() +
  theme(legend.title = element_blank())



# Adding a year column
# co2 <- co2 %>%
#   mutate(Year = year(Timestamp)) %>%
#   group_by(Year) %>%
#   mutate(Start_of_Year = as.POSIXct(paste(Year, "01", "01", sep = "-")),
#          End_of_Year = as.POSIXct(paste(Year, "12", "31", sep = "-"))) %>%
#   ungroup()

# Plot CO2 time-series for all sites w/ no facet wrap
ggplot(sensors, aes(x= Timestamp, y = CO2_cal_uatm, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  theme +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months") +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = "right",
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

# Plotting full sensor time-series w/ facet wrap

ggplot(sensors, aes(x= Timestamp, y = CO2_cal_uatm, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  theme +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months") +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = "right",
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  facet_wrap(~Site_ID, nrow = 3)

ggsave("CO2/graphs/CO2_corr_ppm_no outliers.jpg", width = 10, height = 8, unit = "in")


p1 <- ggplot(sensors) +
  geom_col(aes(x = timestamp_corrected, y = precip_mm), fill = "blue", alpha = 0.75, width = 0.4) +
  scale_y_reverse(expand = c(0,0),
                  limits = c(45, 0)) +
  scale_x_date(expand = c(0,0)) +
  labs(x = "Date", y = "Rainfall \n[mm/day]") +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

p2 <- ggplot(sensors, aes(x = timestamp, y = CO2_cal_uatm, color = Site_ID)) +
  # CO2 lines colored by site
  geom_point(size = 1) +
  labs(x = "Date", y = "CO2 \n[uatm]") +
  scale_y_log10() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    panel.grid = element_blank(),     
    plot.margin = margin(0, 0, 0, 0)
  )

p1 / p2


#### Plots per year ####

# Create a list to store plots
plot_list <- list()

# Generate plots for each year with specific limits
for (year in unique(co2$Year)) {
  start_date <- as.POSIXct(paste(year, "01", "01", sep = "-"))
  end_date <- as.POSIXct(paste(year, "12", "31", sep = "-"))
  
  p <- ggplot(filter(co2, Year == year), aes(x = Timestamp, y = CO2_ppm_corr, color = Site_ID)) +
    geom_line() +
    theme_bw() +
    labs(x = "", y = "CO2 (ppm)") +
    scale_x_datetime(limits = c(start_date, end_date),
                     breaks = seq(from = start_date, to = end_date, by = "2 months"),
                     labels = date_format("%Y-%m"),
                     expand = expansion(mult = c(0, 0))) +
    theme(legend.position = "right") +
    scale_color_manual(values = color_palette) +
    ggtitle(paste("", year))
  
  plot_list[[as.character(year)]] <- p
}

# Display all plots in a grid
CO2_graph <- do.call(grid.arrange, c(plot_list, ncol = 1))

ggsave(plot = CO2_graph, "CO2/graphs/CO2 timeseries by year.jpg", width = 8, height = 8, units = "in")



ggplot(co2, aes(x= Timestamp, y = GP_TempC, color = Site_ID)) +
  geom_line() +
  theme_bw() +
  theme +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "6 months") +
  labs(x= "", y = "Temperature (C)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = "right",
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  facet_wrap(~Site_ID, nrow = 3)

ggsave("CO2/graphs/temperature.jpg")

#### Plotting time-series CO2 + water level data ####

p1 <- ggplot(precip, aes(x= datetime, y = precip_mm)) +
  geom_line() +
  theme +
  labs(x= "", y = "Precipitation (mm)") +
  theme(legend.position='none') +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

ggplot(co2_wl, aes(x = Timestamp_corrected, y = waterLevel, color = Site)) +
  geom_point() +
  theme +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

# Building the time-series graph

ggplot(co2_wl) +
  geom_point(aes(x = Timestamp_corrected, y = CO2_cal_uatm, color = Site)) +
  geom_line(aes(x=Timestamp_corrected, y = waterLevel/10000)) +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months") +
  scale_y_continuous(
    name = "Corrected CO2 (uatm)",
    sec.axis = sec_axis(~ (.*10000), name = "Water Level", labels = scales::comma) # Secondary axis for waterLevel
  ) +
  labs(x = "") +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.position = c(.89, .95),
    legend.justification = c("center", "top"),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  facet_wrap(~Site, nrow = 3)



# Presentation graph

# I think I'm going to stick with the shortened time-series
# There's some cleaning up to do in late 2022 data

ggplot(sensors_short[!is.na(sensors_short$CO2_cal_uatm),], aes(x= Timestamp_corrected, y = CO2_cal_uatm, color = Site)) +
  geom_line() +
  theme_bw() +
  theme +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months") +
  labs(x= "", y = "Dissolved CO2 (uatm)") +
  theme(legend.position='none')

ggsave("Sensor CO2_corr_uatm.jpg")


# Identifying large precipitation events

co2_storm1 <- co2 %>%
  filter(Timestamp > "2021-10-15 00:00") %>%
  filter(Timestamp < "2021-11-15 00:00")

# Plotting storm event

ggplot(co2_storm1, aes(x= Timestamp_corrected, y = CO2_cal_uatm, color = Site)) +
  geom_line() +
  theme_bw() +
  theme +
  labs(x= "", y = "Dissolved CO2 (uatm)") +
  theme(legend.position='none')

ggsave("Sensor CO2_storm 1.jpg")

# Plotting grab samples in sensor gaps

ggplot(co2_clean_subset, aes(x= Timestamp, y = CO2_cal_uatm, color = Site)) +
  geom_line() +
  geom_point(aes(x= as.POSIXct("2021-09-11 00:00"), y = 11820)) +
  geom_point(aes(x= as.POSIXct("2021-09-11 00:00"), y = 6491)) +
  geom_point(aes(x= as.POSIXct("2021-09-11 00:00"), y = 9426)) +
  theme +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.89, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  ylim(0, 25000) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

ggsave("CO2 corrected time-series_JL_subset_w grabs.jpg")


# Plotting full water level time-series for Jackson Lane

ggplot(wl_SW, aes(x = Timestamp, y = waterLevel, color = Site)) +
  geom_line() +
  labs(x = "", y = "Water level (m)") +
  geom_hline(yintercept = 0) +
  labs(x= "", y = "Water level (m)") +
  theme(legend.position = "right") +
  theme_bw() +
  theme +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.89, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months")

ggsave("Water level full_JL.jpg")

# Plotting water level time-series in Jackson Lane for corresponding sensor data

ggplot(wl_SW_subset, aes(x = Timestamp_corrected, y = waterLevel, color = Site)) +
  geom_line() +
  labs(x = "", y = "Water level (m)") +
  geom_hline(yintercept = 0) +
  labs(x= "", y = "Water level (m)") +
  theme(legend.position = "right") +
  theme_bw() +
  theme +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.89, .65),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

ggsave("Water level subset_JL.jpg")

#### Plotting mean/residuals of CO2 ####

# Plotting the residuals of each site around the mean

library(zoo)

# This isn't what I want 

# co2 <- co2 %>%
#   filter(CO2_cal_uatm < 14000) %>%
#   mutate(CO2_1day = rollmean(CO2_cal_uatm, k = 1, fill = NA),
#          CO2_3day = rollmean(CO2_cal_uatm, k = 3, fill = NA))

# Calculate the residuals of each point from the mean
co2$residuals <- co2$CO2_cal_uatm - co2$CO2_3day

# Plot the mean avg across all sites with the +- residuals

co2$WTF <- co2$CO2_cal_uatm + co2$residuals

ggplot(co2) +
  geom_line(aes(x=Timestamp, y = WTF, color = Site)) +
  geom_line(aes(x= Timestamp, y = CO2_3day)) +
  theme_bw() +
  theme +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.89, .95),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")


#### Plotting regressions of CO2 sensor data ####

# CO2 vs water level regression
# For all sites

sensors <- sensors %>%
  mutate(Site_hydroperiod = recode(Site_ID, 
                                   TS = "Short", 
                                   DK = "Medium", 
                                   ND = "Long"))

ggplot(sensors, aes(x= waterLevel, y = CO2_cal_uatm, color = Site_ID)) +
  geom_point(size = 0.5) +
  theme_bw() +
  theme +
  labs(x= "Water level (m)", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position = "none",
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  ylim(0, 25000) +
  facet_wrap(~Site_hydroperiod)

ggsave("CO2 vs wl_JL.jpg")

# Subset by site
ND <- co2_wl %>%
  filter(Site == "ND")

DK <- co2_wl %>%
  filter(Site == "DK")

TS <- co2_wl %>%
  filter(Site == "TS")

JL_co2 <- co2_wl %>%
  filter(Site == c("DK", "TS", "ND"))

# Plot CO2 vs water level for each site

# ND
ggplot(ND, aes(x= waterLevel, y = CO2_cal_uatm, color = Site)) +
  geom_point(color = "#00BA38") +
  theme_bw() +
  geom_smooth(method = "lm") +
  theme +
  labs(x= "Water level (m)", y = "CO2 (uatm)") +
  scale_color_manual(values='dark green') +
  theme(legend.position = "none")

ggsave("CO2 vs WL_ND.jpg")

# DK
ggplot(DK, aes(x= waterLevel, y = CO2_cal_uatm, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme +
  ylim(0, 25000) +
  labs(x= "Water level (m)", y = "CO2 (uatm)") +
  scale_color_manual(values='coral1') +
  theme(legend.position = "none")

ggsave("CO2 vs WL_DK.jpg")

# TS
ggplot(TS, aes(x= waterLevel, y = CO2_cal_uatm, color = Site)) +
  geom_point(color = "#619CFF") +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme +
  labs(x= "Water level (m)", y = "CO2 (uatm)") +
  theme(legend.position = "none")

ggsave("CO2 vs WL_TS.jpg")

summary(lm(CO2_cal_uatm~waterLevel, DK))
summary(lm(CO2_cal_uatm~waterLevel, TS))
summary(lm(CO2_cal_uatm~waterLevel, ND))


# CO2 vs temp regression

ggplot(co2, aes(x = GP_TempC, y= CO2_ppm_corr)) +
  geom_point() +
  xlim(0,40) +
  theme

ggplot(co2_wl, aes(x= GP_TempC, y = CO2_cal_uatm, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme +
  labs(x= "Temperature (C)", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position = c(.4, .99),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6)) +
  xlim(8, 40) +
  ylim(0, 15000)

ggsave("CO2 vs temp_JL.jpg")

anova(lm(CO2_cal_uatm~GP_TempC, co2_wl))

# Plot CO2 vs temp for each site

# ND
ggplot(ND, aes(x= GP_TempC, y = CO2_cal_uatm, color = Site)) +
  geom_point(color = "#00BA38") +
  theme_bw() +
  geom_smooth(method = "lm", color = "black") +
  theme +
  ylim(0, 25000) +
  labs(x= "Temperature (C)", y = "CO2 (uatm)") +
  theme(legend.position = "none")

ggsave("CO2 vs temp_ND.jpg")

# DK
ggplot(DK, aes(x= GP_TempC, y = CO2_cal_uatm, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme +
  ylim(0, 25000) +
  labs(x= "Temperature (C)", y = "CO2 (uatm)") +
  scale_color_manual(values='coral1') +
  theme(legend.position = "none")

ggsave("CO2 vs temp_DK.jpg")

# TS
ggplot(TS, aes(x= GP_TempC, y = CO2_cal_uatm, color = Site)) +
  geom_point(color = "#619CFF") +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme +
  ylim(0, 25000) +
  labs(x= "Temperature (C)", y = "CO2 (uatm)") +
  theme(legend.position = "none")

ggsave("CO2 vs temp_TS.jpg")

summary(lm(CO2_cal_uatm~GP_TempC, DK))
# R2 = 0.15
summary(lm(CO2_cal_uatm~GP_TempC, TS))
# R2 = 0.29
summary(lm(CO2_cal_uatm~GP_TempC, ND))
# R2 = 0.01

# Regressions of temperature vs water level

ggplot(co2_wl, aes(x= waterLevel, y = GP_TempC, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme +
  labs(x= "Water Level (m)", y = "Temperature (C)") +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position = c(.1, .45),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("Temp vs WL_JL.jpg")

summary(lm(GP_TempC~waterLevel, DK))
# R2 = 0.14
summary(lm(GP_TempC~waterLevel, TS))
# R2 = 0.06
summary(lm(GP_TempC~waterLevel, ND))
# R2 = 0.08


#### Plotting boxplots of CO2 sensor data ####

# Boxplot of CO2 across sites

ggplot(co2_clean, aes(x= Site, y= CO2_cal_uatm, color = Site)) +
  geom_boxplot() +
  theme_bw() +
  theme +
  scale_y_log10() +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.65, .5),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("CO2 sensor w outliers_log.jpg")

ggplot(co2_clean, aes(x= Site, y= CO2_cal_uatm, color = Site)) +
  geom_boxplot() +
  theme_bw() +
  theme +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.65, .8),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("CO2 sensor w outliers.jpg")

# Boxplots without high outliers (>20,000)

ggplot(co2_clean2, aes(x= Site, y= CO2_cal_uatm, color = Site)) +
  geom_boxplot() +
  theme_bw() +
  theme +
  scale_y_log10() +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.65, .5),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("CO2 sensor without outliers_log.jpg")


ggplot(co2_clean2, aes(x= Site, y= CO2_cal_uatm, color = Site)) +
  geom_boxplot() +
  theme_bw() +
  theme +
  labs(x= "", y = "CO2 (uatm)") +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size= 18),
        legend.position = c(.65, .85),
        legend.justification = c("center", "top"),
        legend.box.just = "center",
        legend.margin = margin(6, 6, 6, 6))

ggsave("CO2 sensor without outliers.jpg")