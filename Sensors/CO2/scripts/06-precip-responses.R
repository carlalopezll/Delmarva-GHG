# Libraries

library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(zoo)
library(plotly)
library(purrr)

# Should use data from the detrending script so you're not calculating the daily metrics again....

# Read in merged data

sensors <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# Filter for when we have precip

sensors <- filter(sensors, timestamp_corrected < "2021-12-13")

ggplotly(ggplot(sensors, aes(x=Timestamp_corrected, y=CO2_cal_uatm, color=Site_ID)) +
  geom_point())

# Plotting with precipitation

ggplotly(ggplot(sensors, aes(x = timestamp_corrected)) +
  geom_line(aes(y = precip_mm, color = "Precipitation (mm)")) +
  geom_line(aes(y = CO2_cal_uatm / 1000, color = "CO2 (uatm/10)")) +  # Scale CO2 for better visualization
  labs(title = "Time-series of CO2 and Precipitation by Site",
       x = "Time", y = "Scaled Value",
       color = "Variable") +
  facet_wrap(~ Site_ID, ncol = 1) +
  theme_minimal())

ggsave("CO2/graphs/CO2 & precip timeseries.jpg")

### Plotting storm events ####

wl_storm1 <- sensors %>%
  filter(Timestamp > "2021-10-01 00:00") %>%
  filter(Timestamp < "2021-11-30 00:00")

ggplot(wl_storm1) +
  geom_line(aes(x= Timestamp, y= waterLevel, color = Site_Name)) + 
  labs(x = "", y = "Water level (m)") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")


#### Identifying and plotting lags ####

# Subset by site

subset_df <- sensors[, c("CO2_cal_uatm", "precip_mm", "Timestamp_corrected", "Site_ID")]

# Step 2: Split into a list of dataframes by Site
dfs_by_site <- split(subset_df[, c("CO2_cal_uatm", "precip_mm", "Timestamp_corrected")],
                     subset_df$Site_ID)

ND <- dfs_by_site[["ND"]]

pacf <- pacf(ND$CO2_cal_uatm, lag = 20)

pacf_vals <- pacf$acf
lags <- pacf$lag

# Combine into a dataframe
pacf_df <- data.frame(lag = lags, pacf = pacf_vals)

ggplot(pacf_df, aes(x=lag, y = pacf)) +
  geom_point()


co2 <- sensors %>%
  mutate(precip_lag1 = lag(precip_mm, n = 2))

ggplot(co2, aes(x = precip_lag1, y = daily_mean_CO2_cal_uatm)) +
  geom_point() +
  theme_bw() +
  labs(x = "Precipitation (lagged)", y = "CO2 (ppm)") +
  ggtitle("CO2 vs. Lagged Precipitation") +
  facet_wrap(~Site_ID)


# Define lag range
lag_range <- 1:100

# Get results: one row per Site_ID and lag
lagged_models <- sensors %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(function(df_site) {
    site_id <- unique(df_site$Site_ID)
    
    map_dfr(lag_range, function(lag_val) {
      df_lagged <- df_site %>%
        arrange(Timestamp) %>%
        mutate(precip_lag = lag(precip_mm, n = lag_val))
      
      # Fit model
      model <- lm(CO2_cal_uatm ~ precip_lag, data = df_lagged)
      
      data.frame(
        Site_ID = site_id,
        lag = lag_val,
        r_squared = summary(model)$r.squared,
        cor = cor(df_lagged$precip_lag, df_lagged$CO2_cal_uatm, use = "complete.obs")
      )
    })
  })

# Plot R² vs lag
ggplot(lagged_models, aes(x = lag, y = r_squared)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site_ID, scales = "free_y") +
  labs(title = "R² of CO₂ vs Lagged Precipitation per Site",
       x = "Lag (time steps)",
       y = "R² (from linear model)") +
  theme_bw()

best_lags <- lagged_models %>%
  group_by(Site_ID) %>%
  slice_max(r_squared, n = 1, with_ties = FALSE)

print(best_lags)

# Add best lag column back to data for plotting
plot_data <- sensors %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(function(df_site) {
    site_id <- unique(df_site$Site_ID)
    best_lag <- best_lags %>% filter(Site_ID == site_id) %>% pull(lag)
    
    df_site %>%
      arrange(Timestamp) %>%
      mutate(precip_lag = lag(precip_mm, n = best_lag))
  })

# Plot
ggplot(plot_data, aes(x = precip_lag, y = CO2_cal_uatm)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~Site_ID, scales = "free") +
  labs(title = "CO₂ vs. Lagged Precip at Optimal Lag per Site",
       x = "Lagged Precipitation (mm)",
       y = "CO₂ (ppm)") +
  theme_minimal()


#### Detrending ####

library(zoo)

sensors_detrended <- sensors %>%
  arrange(Site_ID, Timestamp) %>%
  group_by(Site_ID) %>%
  mutate(rolling_CO2 = zoo::rollmedian(CO2_cal_uatm, k = 24, fill = NA),
         detrended_CO2 = CO2_cal_uatm - rolling_CO2)

ggplot(sensors_detrended, aes(x = Timestamp)) +
  geom_line(aes(y = CO2_cal_uatm, color = "Original CO2")) +
  geom_line(aes(y = detrended_CO2, color = "Detrended CO2")) +
  labs(title = "Original vs Detrended CO2 Time Series",
       x = "Time",
       y = "CO2 (uatm)",
       color = "CO2 Type") +
  theme_bw()

# Get results: one row per Site_ID and lag
lagged_models <- sensors_detrended %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(function(df_site) {
    site_id <- unique(df_site$Site_ID)
    
    map_dfr(lag_range, function(lag_val) {
      df_lagged <- df_site %>%
        arrange(Timestamp) %>%
        mutate(precip_lag = lag(precip_mm, n = lag_val))
      
      # Fit model
      model <- lm(detrended_CO2 ~ precip_lag, data = df_lagged)
      
      data.frame(
        Site_ID = site_id,
        lag = lag_val,
        r_squared = summary(model)$r.squared,
        cor = cor(df_lagged$precip_lag, df_lagged$detrended_CO2, use = "complete.obs")
      )
    })
  })

# Plot R² vs lag
ggplot(lagged_models, aes(x = lag, y = r_squared)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site_ID, scales = "free_y") +
  labs(title = "R² of CO₂ vs Lagged Precipitation per Site",
       x = "Lag (time steps)",
       y = "R² (from linear model)") +
  theme_bw()

best_lags <- lagged_models %>%
  group_by(Site_ID) %>%
  slice_max(r_squared, n = 1, with_ties = FALSE)

print(best_lags)

# Add best lag column back to data for plotting
plot_data <- sensors_detrended %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(function(df_site) {
    site_id <- unique(df_site$Site_ID)
    best_lag <- best_lags %>% filter(Site_ID == site_id) %>% pull(lag)
    
    df_site %>%
      arrange(Timestamp) %>%
      mutate(precip_lag = lag(precip_mm, n = best_lag))
  })

# Plot
ggplot(plot_data, aes(x = precip_lag, y = detrended_CO2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~Site_ID, scales = "free") +
  labs(title = "CO2 vs. Lagged Precip at Optimal Lag per Site",
       x = "Lagged Precipitation (mm)",
       y = "CO2 (ppm)") +
  theme_minimal()

ggsave("CO2/graphs/lagged precip vs detrended CO2.jpg")


#### Cross-correlation analysis ####

time_to_lags(days = 14, timestep_minutes = 15)

get_ccf_df <- function(x, y, max_lag = 100, site = "Site") {
  ccf_res <- ccf(x, y, lag.max = max_lag, plot = FALSE, na.action = na.pass)
  data.frame(
    lag = ccf_res$lag,
    correlation = ccf_res$acf,
    Site_ID = site
  )
}

ccf_results <- sensors_detrended %>%
  filter(!is.na(detrended_CO2), !is.na(precip_mm)) %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(function(df_site) {
    get_ccf_df(
      x = df_site$precip_mm,
      y = df_site$detrended_CO2,
      max_lag = 1334,
      site = unique(df_site$Site_ID)
    )
  })


ggplot(ccf_results, aes(x = lag, y = correlation)) +
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Site_ID, scales = "free_y") +
  theme_minimal() +
  labs(title = "Cross-Correlation: Precip vs. Detrended CO₂",
       x = "Lag (precip leads →, CO₂ leads ←)",
       y = "Correlation")


# Filter for positive lags only, and then find the maximum correlation at those lags
best_positive_lags <- ccf_results %>%
  filter(lag > 0) %>%
  group_by(Site_ID) %>%
  slice_max(correlation, n = 1, with_ties = FALSE)

# Print the best lags for each site
print(best_positive_lags)

lags_to_time(lag_count = 182, timestep_minutes = 15)
lags_to_time(lag_count = 519, timestep_minutes = 15)

ggplot(best_positive_lags, aes(x = Site_ID, y = correlation, color = Site_ID)) +
  geom_point(size = 3) +
  geom_text(aes(label = paste("Lag", lag)), vjust = -1) +
  labs(title = "Best Positive Lag Correlation for Precip and Detrended CO₂",
       x = "Site",
       y = "Correlation at Best Positive Lag") +
  theme_minimal() +
  theme(legend.position = "none")

#### Starting to play around with ARIMA ####

library(forecast)

# Prepare site-specific time series
site_ts_list <- sensors_detrended %>%
  select(Timestamp, Site_ID, detrended_CO2) %>%
  drop_na(detrended_CO2) %>%
  group_by(Site_ID) %>%
  group_split() %>%
  setNames(map_chr(., ~ unique(.x$Site_ID)))

arima_models <- map(site_ts_list, function(site_df) {
  ts_data <- ts(site_df$detrended_CO2, frequency = 96)  # 96 = 1 day if 15-min intervals
  auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
})

checkresiduals(arima_models[["DK"]])


#### Rolling correlation (window = 10) #### 
ND$rolling_corr <- rollapply(
  data = ND[, c("precip_mm", "CO2_cal_uatm")], 
  width = 10, 
  FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"), 
  by.column = FALSE, 
  align = "right", 
  fill = NA
)

# Plot rolling correlation
ggplot(sensors, aes(x = timestamp_corrected, y = rolling_corr)) +
  geom_line(color = "blue") +
  labs(title = "Rolling Correlation between CO2 and Precipitation",
       x = "Time", y = "Correlation (10-point window)") +
  theme_minimal()


# Example function to analyze response
analyze_response <- function(sensors) {
  sensors <- sensors %>%
    arrange(Site_ID, timestamp_corrected) %>%
    group_by(Site_ID) %>%
    mutate(event = ifelse(precip_mm > 0.2, 1, 0),
           event_id = cumsum(event & (lag(event, default = 0) == 0))) %>%
    ungroup()
  
  # Filter for event occurrences
  event_co2_precip <- sensors %>%
    filter(event == 1) %>%
    group_by(Site_ID, event_id) %>%
    summarize(event_timestamp_corrected = min(timestamp_corrected), .groups = "drop")
  
  # Join to find responses
  response_co2_precip <- event_co2_precip %>%
    left_join(sensors, by = "Site_ID") %>%
    filter(timestamp_corrected >= event_timestamp_corrected) %>%
    group_by(Site_ID, event_id) %>%
    summarize(
      waterLevel_change = max(waterLevel) - waterLevel[timestamp_corrected == event_timestamp_corrected],
      CO2_lag = which.max(CO2_ppm) - which(timestamp_corrected == event_timestamp_corrected),
      CO2_response = max(CO2_ppm) - CO2_ppm[timestamp_corrected == event_timestamp_corrected],
      .groups = "drop"
    )
  
  return(response_co2_precip)
}

# Run analysis
response_results <- analyze_response(sensors)

# View results
print(response_results)

library(tidyr)

response_long <- response_results %>%
  pivot_longer(cols = c(CO2_response, waterLevel_change),
               names_to = "Response_Type",
               values_to = "Value")

ggplot(response_results, aes(x = factor(Site_ID), y = CO2_response)) +
  geom_boxplot(alpha = 0.7, position = position_dodge()) +
  geom_jitter(width = 0) +
  labs(title = "Event Responses per Site",
       x = "Site",
       y = "Value",
       fill = "Site_ID") +
  theme_minimal()

ggplot(response_long, aes(x = factor(Site_ID), y = Value, fill = Response_Type)) +
  geom_boxplot(alpha = 0.7, position = position_dodge()) +
  geom_jitter(width = 0) +
  labs(title = "Event Responses per Site",
       x = "Site",
       y = "Value",
       fill = "Response") +
  theme_minimal()

ggplot(response_results, aes(x = factor(Site_ID), y = CO2_lag_minutes/60)) +
  geom_boxplot(fill = "salmon", alpha = 0.7) +
  labs(title = "CO2 Lag Time per Site",
       x = "Site",
       y = "Lag Time (index difference)") +
  theme_minimal()

ggplot(response_results, aes(x = CO2_lag, y = CO2_response, color = factor(Site_ID))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE  ) +
  labs(title = "CO₂ Response vs Lag Time",
       x = "Lag Time (index)",
       y = "CO₂ Response (ppm)",
       color = "Site") +
  theme_minimal()

# Step: compute time interval (in minutes) per site
library(dplyr)
library(lubridate)

# Estimate time step per site (assuming regular spacing)
time_interval_by_site <- sensors %>%
  arrange(Site_ID, timestamp_corrected) %>%
  group_by(Site_ID) %>%
  summarize(time_step_mins = as.numeric(median(diff(timestamp_corrected)), units = "mins"))

# Join to response results
response_results <- response_results %>%
  left_join(time_interval_by_site, by = "Site_ID") %>%
  mutate(CO2_lag_minutes = CO2_lag * time_step_mins)

ggplot(response_results, aes(x = CO2_lag_minutes, y = CO2_response, color = factor(Site_ID))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "CO₂ Response vs Lag Time (in Minutes)",
       x = "Lag Time (minutes)",
       y = "CO₂ Response (ppm)",
       color = "Site") +
  theme_minimal()


#### Using CCF ####

library(purrr)

# Function to compute CCF for each Site_ID
compute_ccf <- function(data) {
  ccf_result <- ccf(data$precip_mm, data$CO2_cal_uatm, lag.max = 20, plot = TRUE)
  return(ccf_result)
}

# Apply the function by Site_ID
ccf_results <- co2_precip %>%
  group_by(Site_ID) %>%
  group_split() %>%  # Splits data into a list by Site_ID
  map(compute_ccf)




# Function to compute CCF and extract max correlation & corresponding lag
compute_ccf_metrics <- function(data) {
  ccf_result <- ccf(data$precip_mm, data$CO2_cal_uatm, lag.max = 20, plot = FALSE)
  
  # Extract correlation values and lags
  cor_values <- ccf_result$acf[,1,1]  # First dimension holds correlation values
  lags <- ccf_result$lag[,1,1]        # Corresponding lags
  
  # Find the max absolute correlation and its lag
  max_index <- which.max(abs(cor_values))  # Index of max abs correlation
  max_cor <- cor_values[max_index]         # Max correlation value
  best_lag <- lags[max_index]              # Corresponding lag
  
  return(data.frame(Max_Correlation = max_cor, Best_Lag = best_lag))
}

# Apply the function by Site_ID
ccf_summary <- co2_precip %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(compute_ccf_metrics, .id = "Site_ID")

# View the result
print(ccf_summary)

# Create a bar plot
ggplot(ccf_summary, aes(x = factor(Site_ID), y = Max_Correlation, fill = Max_Correlation)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(Max_Correlation, 3)), vjust = -0.5) +
  labs(title = "Max Cross-Correlation between Precipitation and CO₂",
       x = "Site ID",
       y = "Max Correlation") +
  theme_minimal()


library(boot)

# Function to perform permutation test for significance
perm_test <- function(data) {
  observed_ccf <- ccf(data$precip_mm, data$CO2_cal_uatm, lag.max = 20, plot = FALSE)
  max_cor <- max(abs(observed_ccf$acf[,1,1]))  # Max absolute correlation
  
  # Permutation test
  null_distribution <- replicate(1000, {
    shuffled_co2 <- sample(data$CO2_cal_uatm)  # Shuffle CO₂ data
    permuted_ccf <- ccf(data$precip_mm, shuffled_co2, lag.max = 20, plot = FALSE)
    max(abs(permuted_ccf$acf[,1,1]))  # Get max absolute correlation from shuffled data
  })
  
  # Compute p-value
  p_value <- mean(null_distribution >= max_cor)
  
  return(data.frame(Max_Correlation = max_cor, P_Value = p_value))
}

# Apply permutation test by Site_ID
ccf_significance <- co2_precip %>%
  group_by(Site_ID) %>%
  group_split() %>%
  map_dfr(perm_test, .id = "Site_ID")

# Merge with previous results
ccf_summary <- left_join(ccf_summary, ccf_significance, by = "Site_ID")

# View the updated results
print(ccf_summary)
