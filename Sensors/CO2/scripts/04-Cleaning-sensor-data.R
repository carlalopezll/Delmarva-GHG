# Cleaning up sensor data
# Need to do some more work here to clean up outliers and figure out negative values - CLL 6/19/2025

# 1. Outliers
# 2. Getting averages

library(ggplot2)

# Read in csv of corrected sensor data

sensors <- read_csv("CO2/data/processed data/eosGP_JL.csv")

# Outliers
# Should also probably do this by site

out <- boxplot.stats(sensors$CO2_ppm_corr)$out
out_ind <- which(sensors$CO2_ppm_corr %in% c(out))

outliers <- sensors[out_ind, ]

# Plot outlier data

ggplot(outliers, aes(x = Timestamp, y= CO2_ppm_corr)) +
  geom_point()

# Yikes, it's hard to ID high ones
# Outlier analysis IDs the high threshold as 14185.112uatm for CO2_cal_uatm

# Removing low datapoints (assuming they were out of water)
sensors_clean <- filter(sensors, CO2_ppm_corr > 0)

# Removing high datapoints identified by outlier analysis
# sensors_clean <- filter(sensors_clean, CO2_cal_uatm < 14185.112)
# sensors_clean <- filter(sensors_clean, CO2_cal_uatm < 20000)

# Save clean, corrected dataset
write_csv(sensors_clean, "CO2/data/processed data/eosGP_JL_clean.csv")
