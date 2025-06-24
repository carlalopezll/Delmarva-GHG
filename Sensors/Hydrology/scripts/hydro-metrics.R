# Calculating water level metrics


# Read in water level data

wl <- read_csv("Water level & precip/data/Full WL.csv")

# Change the site ID for wl dataframe

wl$Site_ID <- str_extract(wl$Site_Name, "^[^-]+")

# Calculate hydroperiod

# Define the water level threshold
threshold <- 0

hydro_metrics <- wl %>%
  mutate(year = year(Timestamp_corrected),
         is_above_threshold = ifelse(waterLevel > threshold, 1, 0)) %>%
  group_by(Site_ID) %>%
  filter(year < 2022) %>%
  summarise(total_time_above_threshold = sum(is_above_threshold) * 15 / (60 * 24),  # Convert 15-min intervals to days
            total_time = n() * 15 / (60 * 24),  # Total time recorded (in days)
            hydroperiod_avg = total_time_above_threshold / total_time * 100,  # Average hydroperiod as percentage of time
  sd = sd(waterLevel),
  mean = mean(waterLevel),
  median = median(waterLevel),
  cv = (sd/mean)*100)


# Merge hydroperiod information back into the original wl dataset
wl <- wl %>%
  mutate(year = year(Timestamp_corrected)) %>%
  left_join(hydroperiod_summary, by = c("Site_ID", "year"))


ggplot(wl, aes())

# Calculate recession rates

write_csv(wl, "Water level & precip/data/WL_metrics.csv")
