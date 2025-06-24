# Plotting merged sensor data

# Theme
theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

CO2_lab <- expression(paste("C","O"[2]^{}*" (uatm)"))

lims <- lims <- as.POSIXct(strptime(c("2021-04-14 12:30:00", "2024-10-04 00:00"),
                                    format = "%Y-%m-%d %H:%M"))

# Add a column to indicate if water level is increasing or decreasing
sensors2 <- sensors2 %>%
  mutate(
    waterLevel_trend = case_when(
      change_5_days > 0 ~ "Increasing",    # If change is positive
      change_5_days < 0 ~ "Decreasing",    # If change is negative
      TRUE ~ NA_character_                # Handle missing values or other cases
    )
  )

#### Plotting ####

# scale_color_manual(values = c("Spring" = "#009E73", "Summer" = "#F0E442",
#                                 "Fall" = "#D55E00", "Winter" = "#56B4E9"))

# Daily mean CO2 vs daily mean water level

ggplot(sensors, aes(x= daily_mean_wl, y = daily_mean_CO2_cal_uatm, color= season)) +
  geom_point(size = 2) +
  xlim(0, 1) +
  labs(x = "Daily mean water level (m)", y = CO2_lab, color = "Season") +
  scale_y_log10() +
  scale_color_paletteer_d("fishualize::Acanthurus_coeruleus") +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ Site_hydroperiod, ncol= 3) +
  theme +
  theme(legend.position = "none", 
        strip.text = element_text(size = 20))

ggsave("CO2/graphs/CO2-WL by season.jpg", width = 18, height = 8, units = "in")
# ggsave("CO2/graphs/CO2 vs WL_daily mean_long.jpg", width = 12, height = 16, units = "in")

lm_CO2_wl <- sensors %>%
  group_by(Site_ID) %>%
  do({
    model <- lm(daily_mean_CO2_cal_uatm ~ daily_mean_wl, data = .)
    model_summary <- summary(model)
    tidy_model <- tidy(model)
    tidy_model$r_squared <- model_summary$r.squared
    tidy_model$r_squared_adj <- model_summary$adj.r.squared
    tidy_model
  })


ggplot(sensors2, aes(x= Site_ID, y = CO2_ppm_corr, fill = Site_ID)) +
  geom_violin()

ggplot(sensors2, aes(x = Site_ID, y = CO2_ppm_corr, fill = Site_ID)) +
  geom_violin(trim = FALSE) +  # Set trim to FALSE to show the full distribution
  geom_boxplot(width = 0.1, position = position_dodge(0.9), alpha = 0.5) +  # Optional: add boxplots inside violins
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  labs(x = "Site ID", y = "CO2 (ppm)", title = "Distribution of CO2 Levels by Site ID") +
  theme_minimal() +  # Use a minimal theme for better aesthetics
  theme(legend.position = "none")  # Remove legend if not needed

medians <- sensors2 %>%
  group_by(Site_ID) %>%
  summarize(median_CO2 = median(CO2_ppm_corr, na.rm = TRUE), .groups = 'drop')

ggplot(sensors2, aes(x = CO2_ppm_corr, y = ..density.., fill = Site_ID)) +
  geom_density(alpha = 0.5, adjust = 1.5) +  # Density estimation
  geom_vline(data = medians, aes(xintercept = median_CO2, color = Site_ID), linetype = "dashed", size = 1) +  
  labs(x = "CO2 (ppm)", y = "Density", title = "") +
  theme_minimal() +  # Minimal theme for better aesthetics
  theme(legend.position = "right")  # Position legend

ggsave("CO2/graphs/Sensor CO2 distribution.jpg")


ggplot(sensors2, aes(x= waterLevel, y = CO2_ppm_corr, color= season)) +
  geom_point() +
  xlim(0, 1) +
  labs(x = "Water level (m)", y = "CO2 (ppm)") +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(label = paste(after_stat(rr.label))), 
           method = "pearson", 
           label.x = 0.7, 
           label.y = 30000, 
           size = 5, 
           color = "black") +
  facet_wrap(~ Site_ID) +
  theme

ggplot(sensors2, aes(x= daily_mean_wl, color= season)) +
  geom_point(aes(y = CO2_ppm_corr), color = "grey", alpha = 0.1) +
  # Plot daily_mean_CO2_ppm_corr with higher opacity and in front
  geom_point(aes(y = daily_mean_CO2_ppm_corr), alpha = 0.8) +
  xlim(0, 1) +
  labs(x = "Water level (m)", y = "CO2 (ppm)") +
  geom_smooth(aes(y = daily_mean_CO2_ppm_corr), method = "lm", color = "black") +
  facet_wrap(~ Site_ID) +
  theme

ggsave("CO2/graphs/CO2 vs WL_both.jpg", width = 17, height = 7, units = "in")


sensors2_filtered <- sensors2 %>%
  filter(!is.na(waterLevel_trend))

ggplot(sensors2_filtered, aes(x= daily_mean_wl, y = daily_mean_CO2_ppm_corr, color= waterLevel_trend)) +
  geom_point(size = 3) +
  xlim(0, 1) +
  labs(x = "Water level (m)", y = "CO2 (ppm)") +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(group = Site_ID, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0.5, size = 5) +
  facet_wrap(~ Site_ID) +
  theme +
  theme(legend.position = "bottom")


ggplot(sensors2_filtered, aes(x= waterLevel_trend, y = daily_mean_CO2_ppm_corr)) +
  geom_boxplot() +
  facet_wrap(~Site_ID)


ggplot(sensors2, aes(x= change_5_days, y = CO2_ppm_corr, color= season)) +
  geom_point() +
  geom_vline(xintercept=0) +
  labs(x = "Daily change in water level (m)", y = "CO2 (ppm)") +
  scale_color_manual(values = c("Spring" = "#009E73", "Summer" = "#F0E442",
                                "Fall" = "#D55E00", "Winter" = "#56B4E9")) +
  facet_wrap(~ Site_ID) +
  theme +
  theme(legend.position = "bottom")


ggsave("CO2/graphs/CO2 vs WL change.jpg", width = 17, height = 8, units = "in")




ggplot(sensors2, aes(x= waterLevel, y = DO_conc_mgL)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(label = paste(after_stat(rr.label))), 
           method = "pearson", 
           label.x = 0.6, 
           label.y = 8, 
           size = 5, 
           color = "black") +
  facet_wrap(~ Site_ID)


#### Time-series ####

ggplot(sensors2, aes(x= Timestamp, y = waterLevel, color = Site_ID)) +
  geom_point() +
  theme +
  labs(x ="", y = "Water level (m)")

# CO2 timeseries for all 3 sites
ggplot(sensors3, aes(x= Timestamp, y = CO2_ppm_corr, color = Site_ID)) +
  geom_point() +
  theme +
  labs(x ="", y = "CO2 (ppm)")

ggsave("CO2/graphs/CO2 timeseries_points.jpg", width = 10, height = 6, units = "in")



# Calculate counts for each month
sensors3_count <- sensors3 %>%
  group_by(month) %>%
  summarize(count = n()) %>%
  mutate(month_label = paste(month, "(n=", count, ")"))

# Join the count back to the original data
sensors3 <- sensors3 %>%
  left_join(sensors3_count, by = "month")

ggplot(sensors3, aes(x= DO_conc_mgL, y = CO2_HiConc_ppm, color = month)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(label = paste(after_stat(rr.label))), 
           method = "pearson", 
           label.x = 0.6, 
           label.y = 8, 
           size = 5, 
           color = "black") +
  facet_wrap(~Site_ID) +
  theme +
  labs(x ="DO (mg/L)", y = "CO2 (ppm)")

ggsave("CO2/graphs/CO2 vs. DO.jpg", width = 14, height = 8, units = "in")

model <- lm(CO2_HiConc_ppm ~ DO_conc_mgL * Site_ID, data = sensors3)

# Summary of the model
summary(model)



means <- sensors3 %>%
  group_by(Site_ID, Timestamp) %>%
  summarize(mean_DO = mean(DO_conc_mgL, na.rm = TRUE),
            mean_CO2 = mean(CO2_HiConc_ppm, na.rm = TRUE),
            .groups = 'drop')

# Plot
ggplot(sensors3, aes(x = DO_conc_mgL, y = CO2_HiConc_ppm, color = month)) +
  geom_point(alpha = 0.5) +  # Adjust alpha for better visibility of density
  geom_smooth(method = "lm", color = "black") +  # Mean points
  stat_cor(aes(label = paste(after_stat(rr.label))), 
           method = "pearson", 
           label.x = 0.6, 
           label.y = 8, 
           size = 5, 
           color = "black") +
  geom_density_2d(aes(color = month), size = 0.5) +  # Add density cloud
  facet_wrap(~Site_ID) +
  theme +
  labs(x ="DO (mg/L)", y = "CO2 (ppm)", color = "Month")



results <- sensors3 %>%
  group_by(Site_ID) %>%
  do(tidy(lm(CO2_ppm_corr ~ DO_conc_mgL, data = .)))


ggplot(sensors3, aes(x= Logger_TempC.y, y= daily, color = month)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(label = paste(after_stat(rr.label))), 
           method = "pearson", 
           label.x = 0.6, 
           label.y = 8, 
           size = 5, 
           color = "black") +
  facet_wrap(~Site_ID) +
  theme +
  labs(x ="Temperature (C)", y = "CO2 (ppm)")

ggsave("CO2/graphs/CO2 vs temperature.jpg", width = 14, height = 8, units = "in")


results <- sensors2 %>%
  group_by(Site_ID) %>%
  do(tidy(lm(CO2_ppm_corr ~ Logger_TempC.y, data = .)))

print(results)


long_data <- sensors2 %>%
  pivot_longer(cols = c(CO2_HiConc_ppm, DO_conc_mgL, waterLevel), 
               names_to = "Variable", 
               values_to = "Value")

# Plot
ggplot(long_data, aes(x = Timestamp, y = Value, color = Site_ID)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "Time", y = "Value", title = "Time Series of CO2, DO, and Water Level") +
  theme_minimal() +
  theme(legend.position = "right")

precip$precip_mm <- as.numeric(precip$precip_mm)

count_precip_above_one <- precip %>%
  summarise(count = sum(precip_mm > 1, na.rm = TRUE))

print(count_precip_above_one)

ggplot(precip, aes(x = precip_mm)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_log10()

ggsave("Water level & precip/graphs/distribution of precip.jpg")

precip_storm <- precip %>%
  filter(datetime > "2021-10-25 00:00") %>%
  filter(datetime < "2021-10-28 00:00")

precip_storm2 <- precip %>%
  filter(datetime > "2021-06-09 00:00") %>%
  filter(datetime < "2021-06-11 00:00")

storm1 <- sensors2 %>%
  filter(Timestamp > "2021-10-25 00:00") %>%
  filter(Timestamp < "2021-10-28 00:00")

storm2 <- sensors2 %>%
  filter(Timestamp > "2021-06-09 00:00") %>%
  filter(Timestamp < "2021-06-11 00:00")

ggplot(precip_storm2, aes(x= datetime, y = precip_mm)) +
  geom_line() +
  theme

ggsave("CO2/graphs/storm2_precip.jpg")

ggplot(storm2, aes(x= Timestamp, y = CO2_HiConc_ppm, color = Site_ID)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "CO2 (ppm)") +
  theme +
  geom_vline(xintercept = as.POSIXct("1970-01-01 00:00:00"), linetype = "dashed", color = "red") +  # Example for a specific date
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d %H:%M")

ggsave("CO2/graphs/storm2_CO2.jpg")



ggplot(storm2, aes(x= waterLevel, y= CO2_HiConc_ppm, color = Timestamp)) +
  geom_point() +
  theme +
  facet_wrap(~Site_ID, scales = "free_x") +
  labs(x = "Water level (m)", y = "CO2 (ppm)")

ggsave("CO2/graphs/storm2_CO2 vs WL.jpg", width = 10, height = 8, units = "in")
