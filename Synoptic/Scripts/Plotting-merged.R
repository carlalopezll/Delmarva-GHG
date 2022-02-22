# Plotting merged data set

library(dplyr)
library(ggplot2)
library(methods) # for geom_stat
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr) # for p values and r-squared on graph

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data
load('All_GHG.RData') # GHG data merged across all synoptic dates

full_field <- read_csv("full_field.csv") # Field data

all <- read.csv("All_synoptic.csv") # Other variables analyzed
all_clean <- filter(all, !(Sample_Type == "RI") & !(Sample_Type == "CH")) # Remove CH and RI

# Subset by sample type

# Just GHG data
SW <- filter(GHG_all, Sample_Type == "SW")
UW <- filter(GHG_all, Sample_Type == "UW")
RI <- filter(GHG_all, Sample_Type == "RI")
CH <- filter(GHG_all, Sample_Type == "CH")

# Merged GHG data with other analytes
all_SW <- filter(all, Sample_Type == "SW")
all_UW <- filter(all, Sample_Type == "UW")
all_RI <- filter(all, Sample_Type == "RI")
all_CH <- filter(all, Sample_Type == "CH")


# Plotsplotsplots

# Theme stuff
theme <- theme(axis.text = element_text(size = "12"), axis.title = element_text(size=15))
# theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                    axis.line = element_line(colour = "black"))

# Sample type vs GHG
ggplot(GHG_all, aes(x = Sample_Type, y = CO2_uM)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland Surface water", "UW" = " Wetland Groundwater")) +
  theme(axis.text.x = element_text(angle=25), axis.title = element_text(size=15)) +
  geom_jitter()

ggsave("Graphs/CO2 vs sample type.jpg")

ggplot(GHG_all, aes(x = Sample_Type, y = CH4_uM)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland Surface water", "UW" = " Wetland Groundwater")) +
  theme(axis.text.x = element_text(angle=25), axis.title = element_text(size=15)) +
  geom_jitter()

ggsave("Graphs/CH4 vs sample type.jpg")


# Boxplots of GHG across all sites
ggplot(SW, aes(x = Site, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )
  
ggsave("Graphs/CH4_SW_boxplot.jpg")

ggplot(SW, aes(x = Site, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CO2_SW_boxplot.jpg")

ggplot(UW, aes(x = Site, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CH4_UW_boxplot.jpg")

ggplot(UW, aes(x = Site, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  theme(
    legend.position = c(.86, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CO2_UW_boxplot.jpg")

# For Jackson Lane sites only 

# ggplot(GHG_SW_JL, aes(x = Site, y = CO2_uM)) +
#   geom_boxplot() +
#   geom_jitter() +
#   theme(
#     legend.position = c(.86, .99),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6),
#     axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
#   )


#### Regressions w/ other water chem and physicochemistry ####

ggplot(all_clean, aes(x = NPOC_mgC_L, y = CO2_uM, color = Sample_Type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 30) +
  theme +
  theme(legend.position = c(0.99, .90), # For legend position
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

summary(lm(formula = CO2_uM~NPOC_mgC_L, data = all_SW)) # almost significant
summary(lm(formula = CO2_uM~NPOC_mgC_L, data = all_UW)) # significant

ggsave("Graphs/CO2 vs DOC_all.jpg")

ggplot(all_clean, aes(x = NPOC_mgC_L, y = CH4_uM, color= Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm")+
  theme(legend.position = c(.86, .90), # For legend position
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

summary(lm(formula = CH4_uM~NPOC_mgC_L, data = all_SW)) # significant
summary(lm(formula = CH4_uM~NPOC_mgC_L, data = all_UW)) # not significant

ggsave("Graphs/CH4 vs DOC_all.jpg")

# Just surface water: CO2

ggplot(all_SW, aes(x = NPOC_mgC_L, y = CO2_uM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("DOC (mg/L)") + ylab("CO2 concentrations (uM)") +
  theme

ggsave("Graphs/CO2 vs DOC_SW.jpg")

# Just surface water: CH4

ggplot(all_SW, aes(x = NPOC_mgC_L, y = CH4_uM)) +
  geom_point() +
  xlab("DOC (mg/L)") + ylab("CH4 concentrations (uM)") +
  theme

ggsave("Graphs/CH4 vs DOC_SW.jpg")

# CH4 vs CO2
ggplot(all_clean, aes(x = CO2_uM, y = CH4_uM, color= Sample_Type)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme(
    legend.position = c(.86, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

ggsave("Graphs/CH4 vs CO2_all.jpg")

# Load spreadsheet that has field data merged with GHG data

# Filter SW and GW

full_clean <- filter(full_field, !(Sample_Type == "RI") & !(Sample_Type == "CH")) # Remove CH and RI

full_SW <- filter(full_field, Sample_Type == "SW")
full_UW <- filter(full_field, Sample_Type == "UW")

ggplot(full_clean, aes(x= DO_percent, y = CO2_uM, color = Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm") +
  theme(legend.position = c(.86, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

summary(lm(formula = CO2_uM~DO_percent, data = full_SW)) # significant
summary(lm(formula = CO2_uM~DO_percent, data = full_UW)) # almost significant

ggsave("Graphs/CO2 vs DO_all.jpg")

ggplot(full_clean, aes(x= DO_percent, y = CH4_uM, color = Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm") +
  theme(legend.position = c(.86, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

ggsave("Graphs/CH4 vs DO_all.jpg")

summary(lm(formula = CH4_uM~DO_percent, data = full_SW)) # significant
summary(lm(formula = CH4_uM~DO_percent, data = full_UW)) # not significant

# Just surface water: CO2
ggplot(full_SW, aes(x= DO_percent, y = CO2_uM)) +
  geom_point() +
  theme +
  xlab("Dissolved oxygen (%)") + ylab("CO2 concentrations (uM)") +
  geom_smooth(method = "lm")

ggsave("Graphs/CO2 vs DO_SW.jpg")
# Just surface water: CH4
ggplot(full_SW, aes(x= DO_percent, y = CH4_uM)) +
  geom_point() +
  theme +
  xlab("Dissolved oxygen (%)") + ylab("CH4 concentrations (uM)") +
  geom_smooth(method = "lm")

ggsave("Graphs/CH4 vs DO_SW.jpg")


# Temperature #


ggplot(full_clean, aes(x= Temp_C, y = CH4_uM, color = Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm") +
  theme(legend.position = c(.86, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

summary(lm(formula = CH4_uM~Temp_C, data = full_SW))
ggsave("Graphs/CH4 vs temp_all.jpg")

ggplot(full_SW, aes(x= Temp_C, y = CH4_uM)) +
  geom_point() +
  theme +
  stat_smooth(method = "loess")

ggsave("Graphs/CH4 vs temp_SW.jpg")

ggplot(full, aes(x= Temp_C, y = CO2_uM, color = Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm") +
  theme(
    legend.position = c(.86, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

summary(lm(formula = CO2_uM~Temp_C, data = full_UW))

ggsave("Graphs/CO2 vs temp_all.jpg")

