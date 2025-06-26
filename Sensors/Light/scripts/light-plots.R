# Script for plotting light data from Delmarva wetlands
# Carla López Lloreda

# load libraries
library(ggplot2)
library(dplyr)

# read in data
light <- read.csv("Light/data/light_data.csv")

# fix datetime
light$datetime_corrected <- as.POSIXct(light$datetime, format = "%m/%d/%y %I:%M:%S %p")

# time-series for the 3 sites
ggplot(light, aes(x=datetime_corrected, y = PAR)) +
  geom_line() +
  facet_wrap(~site, ncol= 1) +
  labs(x = "Datetime", y = "PAR (umol/m2/s)")

# boxplot
ggplot(light, aes(x= site, y = PAR)) +
  geom_boxplot()

# getting summary stats
summary <- light %>%
  group_by(site) %>%
  summarize(PAR_mean = mean(PAR, na.rm = TRUE))