# Script for plotting GW-SW transect data from Delmarva
# Carla López Lloreda
# Created 2/21/2022

library(dplyr)
library(ggplot2)
library(methods) # for geom_stat
library(tidyverse)
library(hrbrthemes)
library(viridis)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data
load('All_GHG.RData') # GHG data merged across all synoptic dates

full_field <- read_csv("full_field.csv") # Field data

all <- read.csv("All_synoptic.csv") # Other variables analyzed
all <- filter(all, !Sample_Type == "RI" | !Sample_Type == "CH") # Remove CH and RI samples types


# Subset triangulated transects
transect <- filter(GHG_all, Site == "ND" | Site == "DK" | Site == "QB") %>%
  filter(!Site_ID == "ND-outlet", !Site_ID == "DK-CH", !Site_ID == "ND-UW3")

# Rearrange factor order from upstream to downstream in transect subset
transect$Site_ID <- factor(transect$Site_ID,levels = c("ND-UW1", "ND-SW", "ND-UW2",
                                                       "DK-UW1", "DK-SW", "DK-UW2",
                                                       "QB-UW1", "QB-SW", "QB-UW2"))

# Plot transect data

ggplot(transect, aes(x= Site_ID, y = CO2_uM, fill = Site)) +
  geom_boxplot() +
  facet_wrap(~Site, scales="free") +
  theme(legend.position = "none",
        axis.text = element_text(size = "12"), axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45))

ggsave("Graphs/CO2_transect.jpg")

ggplot(transect, aes(x= Site_ID, y = CH4_uM, fill = Site)) +
  geom_boxplot() +
  facet_wrap(~Site, scales="free") +
  theme(legend.position = "none",
        axis.text = element_text(size = "12"), axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45)) +
  ylim(0,65)

ggsave("Graphs/CH4_transect.jpg")