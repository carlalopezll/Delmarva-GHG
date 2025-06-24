# Rain events and hysteretic responses


# Calculate the daily amplitude in CO2 and relate it to...
# sensors$delta_co2 <- 

ND$timestamp_corrected <- as.Date(ND$timestamp)

ND <- ND %>%
  group_by(timestamp_corrected) %>%
  mutate(precip_sum = cumsum(precip_mm))

ggplot(ND, aes(x= timestamp_corrected, y = precip_sum)) +
  geom_col(fill = "blue", alpha = 0.75, width = 0.4) +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_date(expand = c(0,0)) +
  labs(x = "Date", y = "Rainfall \n[mm/day]") +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

library(HyMETT)
POR_calc_amp_and_phase(data = ND, Date = "timestamp_corrected", value = "GP_TempC") # i don't even know what this is doing

sensors <- ND
  
# Flag rainy time points
sensors <- sensors %>%
  mutate(rain_flag = ifelse(is.na(precip_mm), FALSE, precip_mm > 0))

rle_rain <- rle(sensors$rain_flag)
event_ids <- rep(NA, length(sensors$rain_flag))

event_counter <- 1
idx <- 1
for (i in seq_along(rle_rain$lengths)) {
  len <- rle_rain$lengths[i]
  val <- rle_rain$values[i]
  
  if (isTRUE(val)) {
    event_ids[idx:(idx + len - 1)] <- event_counter
    event_counter <- event_counter + 1
  }
  
  idx <- idx + len
}
sensors$rain_event_id <- event_ids

# Pick a rain event to visualize
event_id_to_plot <- 3  # update as needed
event_df <- sensors %>%
  filter(rain_event_id == event_id_to_plot)

# Skip if no event found
if (nrow(event_df) == 0) stop("No data for that event ID.")


event_df <- event_df %>%
  mutate(
    time_since_start = as.numeric(difftime(timestamp, min(timestamp), units = "hours")),
    cum_rain_mm = cumsum(precip_mm)
  )

ggplot(event_df, aes(x = cum_rain_mm, y = CO2_cal_uatm, color = Site_ID)) +
  geom_path(color = "blue", arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  geom_point(aes(color = time_since_start)) +
  labs(title = paste("Hysteresis of CO2 during Rain Event", event_id_to_plot),
       x = "Cumulative Rain (mm)",
       y = expression(CO[2]~(mu*atm)),
       color = "Time since start (h)") +
  theme_minimal()


ggplot(sensors, aes(x = cum_rain_mm, y = CO2_cal_uatm, color = Site_ID)) +
  geom_path(color = "blue", arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  geom_point(aes(color = time_since_start)) +
  labs(title = paste("Hysteresis of CO2 during Rain Event", event_id_to_plot),
       x = "Cumulative Rain (mm)",
       y = expression(CO[2]~(mu*atm)),
       color = "Time since start (h)") +
  theme_minimal()
