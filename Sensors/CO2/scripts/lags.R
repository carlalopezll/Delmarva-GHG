

# identifying best lags

ccf_data_daily <- sensors_daily2 %>%
  filter(Site_ID == "ND") %>%
  select(Site_ID, daily_precip, daily_wl) %>%
  drop_na()

ccf_data_hf <- sensors_hf %>%
  filter(Site_ID == "ND") %>%
  select(Site_ID, precip_mm, waterLevel) %>%
  drop_na()

# 15-minute intervals
# lag 4 = 1 hour
# lag 96 = 1 day

acf(ccf_data_hf$precip_mm)

ccf_result_hf <- ccf(ccf_data_hf$precip_mm, ccf_data_hf$waterLevel, lag.max = 90, plot = TRUE) # lag is in 15 minute intervals
ccf_result_daily <- ccf(ccf_data_daily$daily_precip, ccf_data_daily$daily_wl, lag.max = 10, plot = TRUE) # lag is in days

# Step 1: Create lagged precip variables
sensors_lagged <- sensors_hf %>%
  group_by(Site_ID) %>%
  arrange(date) %>%
  mutate(
    precip_lag0 = precip_mm,
    precip_lag4 = lag(precip_mm, 4),
    precip_lag12 = lag(precip_mm, 12),
    precip_lag24 = lag(precip_mm, 100)
  )

# Step 2: Reshape to long format
sensors_long <- sensors_lagged %>%
  pivot_longer(cols = starts_with("precip_lag"),
               names_to = "lag",
               values_to = "precip_lagged")

# Step 3: Plot with facets
ggplot(sensors_long, aes(x = precip_lagged, y = waterLevel, color = Site_ID)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~lag) +
  labs(title = "Effect of Lagged Precipitation on Water Level",
       x = "Lagged Precipitation (mm)",
       y = "Water Level") +
  theme_minimal()


# Extract lags (in time units of your input, e.g., 15-min intervals)
lags <- ccf_result_hf$lag

# Extract cross-correlation coefficients
acf_values <- ccf_result_hf$acf

# Combine into a data frame for easy use
ccf_df <- data.frame(
  lag = as.numeric(lags),   # convert from 'ts' structure
  acf = as.numeric(acf_values)
)

ccf_df %>%
  filter(abs(acf) == max(abs(acf)))  # lag with strongest correlation

ggplot(ccf_df, aes(x = lag, y = acf)) +
  geom_line() +
  geom_hline(yintercept = c(-1.96/sqrt(nrow(ccf_df)), 1.96/sqrt(nrow(ccf_df))), linetype = "dashed", color = "blue") +
  labs(title = "Cross-Correlation: Precipitation vs. Water Level",
       x = "Lag (15-min intervals)", y = "Cross-correlation") +
  theme_minimal()
