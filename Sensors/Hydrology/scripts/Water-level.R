# Delmarva water level data
# Merging and plotting water level time-series
# Created by Carla Lopez Lloreda on 3/18/2022
# Last updated 4/18/2023

library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
library(plotly)
library(readr)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Water level")

# #### Merging data files #### 
# 
# # Read in all the water level files
# wl_20200508 <- read_csv("output/output_20200508_JM.csv")
# wl_20201015 <- read_csv("output/output_20201015_JM.csv")
# wl_20210525 <- read_csv("output/output_20210525_JM.csv")
# wl_20211112 <- read_csv("output/output_20211112_JM.csv")
# wl_20220410 <- read_csv("output/output_20220410_JM.csv")
# 
# # Bind WL files
# wl <- rbind(wl_20200508, wl_20201015, wl_20210525, wl_20211112, wl_20220410)
# 
# # Fix date
# 
# wl$Timestamp_corrected <- parse_date_time(wl$Timestamp, "Ymd HMS", truncated = 3)
# 
# # Add a sample type column so you can then subset only SW/GW
# 
# wl$Sample_Type <- substr(wl$Site_Name, start= 4, stop= 5)
# 
# # Remove crappy TB data
# # Can't figure out the above, just remove TB for now
# 
# wl <- filter(wl, !Site_Name == "TB-SW")
# 
# # Save file
# 
# write.csv(wl, "DMV WL.csv", row.names = FALSE)

############################################################

# Read in if not adding any new data above
wl <- read.csv("output/output_JM_2019_2022.csv")

# Filter out flagged data

wl <- wl %>%
  filter(Flag == 0)

# This is the spreadsheet for daily average water level
# wl <- read_csv("output/dly_mean_output_JM_2019_2022.csv")

wl$Sample_Type <- substr(wl$Site_Name, start= 4, stop= 5)

# Fix timestamp
wl$Timestamp_corrected <- parse_date_time(wl$Timestamp, "Ymd HMS", truncated = 3)

# Subset SW
wl_SW <- subset(wl, Sample_Type %in% "SW")

# Subset GW # need to decide what GW well to use
wl_UW <- subset(wl, Sample_Type %in% "UW")


#### Plotting ####

# Theme stuff
# theme <- theme(axis.text = element_text(size = "16"), axis.title = element_text(size=30))

theme <- theme_bw() +
  theme(text = element_text(size = 20), legend.position = "none")

# Blank water level time-series

ggplot(wl_SW, aes(x= Timestamp_corrected, y= waterLevel, color= Site_Name)) +
  geom_hline(yintercept = 0) +
  labs(x= "", y = "Water level (m)") +
  theme +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

# Plotting only one site

ggplot(wl[wl$Site_Name %in% "ND-SW", ], aes(x= Timestamp, y= waterLevel)) +
  geom_point()

# Plot WL time-series for all the sites

ggplot(wl_SW, aes(x= Timestamp_corrected, y= waterLevel, color= Site_Name)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(x= "", y = "Water level (m)") +
  theme(legend.position = "right") +
  theme +
  theme_bw() +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "2 months")

ggsave("WL SW timeseries.jpg")

# Plotting on the same time frame as synoptic sampling

lims <- as.POSIXct(strptime(c("2020-10-30 00:00", "2022-12-13 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

# Highlighting sampling dates on graph

sampling_dates <- as.POSIXct(strptime(c("2020-10-31 00:00", "2021-02-25 00:00", "2021-05-09 00:00", "2021-06-19 00:00", "2021-09-11 00:00", "2021-10-19 00:00", "2021-11-14 00:00", "2021-12-13 00:00", "2022-03-31 00:00", "2022-10-31 00:00", "2022-12-13 00:00"), format = "%Y-%m-%d %H:%M"))

# Getting an average of water levels

detach(package:dplyr)
library(dplyr)

wl_SW_avgs <- wl_SW %>%
  group_by(Timestamp_corrected = Timestamp_corrected) %>%
  summarise(Mean = mean(waterLevel), Timestamp_corrected = first(Timestamp_corrected))


wl_SW_avgs <- wl_SW %>%
  group_by(Timestamp_corrected) %>%
  summarise(Mean = mean(waterLevel),
            Min = min(waterLevel),
            Max = max(waterLevel),
            Sd = sd(waterLevel),
            Timestamp_corrected = first(Timestamp_corrected))


# Plotting the average water level w/ geom_ribbon

wl_graph <- ggplot(wl_SW_avgs, aes(x= Timestamp_corrected, y = Mean), size = 2) +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd), alpha = 0.3, fill = "blue") +
  labs(x = "", y = "Water level (m)", tag = "a)") +
  ylim(-0.2,1.3) +
  geom_hline(yintercept = 0, size = 1) +
  theme_bw() +
  theme +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  # adding sampling date lines
  geom_vline(xintercept = as.numeric(sampling_dates), linetype=4, size= 1) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months",
                   limits = lims)

wl_graph

ggsave("Water level mean w/ ribbon.jpg")

# Merge full water level data with the time averages
# Should probably save this instead of doing it over again

wl_SW_join <- inner_join(wl_SW, wl_SW_avgs, by = c("Timestamp_corrected"))

# Plot water level for all sites + an average
# To match with synoptic sampling

wl_graph <- ggplot(wl_SW_join) +
  geom_ribbon(aes(x = Timestamp_corrected, ymin = waterLevel - StdDev, ymax = waterLevel + StdDev, fill = Site_Name), alpha = 0.3) +
  geom_line(aes(x= Timestamp_corrected, y = Mean), size = 1.5) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month", limits = lims) +
  labs(x = "", y = "Water level (m)", tag = "a)") +
  ylim(-0.2,1.3) +
  geom_hline(yintercept = 0, size = 1) +
  theme_bw() +
  theme +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  # adding sampling date lines
  geom_vline(xintercept = as.numeric(sampling_dates), linetype=4, size= 1)

wl_graph

ggsave("Graphs/WL SW timeseries_w avg.jpg")

# Same as previous but without the vertical sampling lines

ggplot(wl_SW_join) +
  geom_line(aes(x= Timestamp_corrected, y= waterLevel, color = Site_Name)) +
  geom_line(aes(x= Timestamp_corrected, y = Mean), size = 2) +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months",
                   limits = lims) + 
  labs(x = "", y = "Water level (m)") +
  ylim(-0.2,1.3) +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() +
  theme +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggsave("Graphs/WL SW timeseries_w avg.jpg")

# Plotting just the average water level

ggplot(wl_SW_join) +
  geom_line(aes(x= Timestamp_corrected, y = Mean, size = 1)) +
  scale_x_datetime(labels = date_format("%Y-%m-%d"), date_breaks = "1 months",
                   limits = lims) + 
  labs(x = "", y = "Water level (m)") +
  ylim(-0.2,1.3) +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() +
  theme +
  theme(legend.position = "none", axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  
  # adding sampling date lines
  geom_vline(xintercept = as.numeric(sampling_dates), linetype=4, size = 2)

ggsave("WL SW timeseries_average.jpg")
