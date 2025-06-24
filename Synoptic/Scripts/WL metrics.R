# Water level analyses + metrics


# Create an increasing/decreasing WL variable

# Calculate the difference in water level over the past 5 days


SW <- SW %>%
  arrange(Site, Date_corrected)

calculate_change <- function(x) {
  c(NA, diff(x, lag = 2))
}

calculate_change <- function(x) {
  if (length(x) < 6) {
    return(rep(NA, length(x)))
  } else {
    return(c(rep(NA, 5), diff(x, lag = 5)))
  }
}

SW <- SW %>%
  group_by(Site) %>%
  mutate(change_in_water_level = calculate_change(dly_mean_wtrlvl))