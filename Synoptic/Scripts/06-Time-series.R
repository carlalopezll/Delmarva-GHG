# Delmarva time series
# Carla López Lloreda
# This includes time-series for CO2, CH4, CH4:CO2, DO

library(dplyr)
library(ggplot2)
library(methods) # for geom_stat
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(scales)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)
library(gridExtra)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

precip <- read.csv("Jackson Lane precip.csv")
precip$Date_corrected <- as.Date(precip$timestamp)


# Read in merged
merge <- read.csv("Master spreadsheet.csv")

# Fix date
merge$Date_corrected <- as.Date(parse_date_time(merge$Date_corrected, c("mdy", "ymd")))

# Rearrange factor order by site/watershed

merge$Site <- factor(merge$Site,levels = c("ND", "BD", "TS", "DK", "FR", # Jackson Lane
                                           "TA", "TB", "DB", "FN", # Tiger Paw/Beetree Rd (Jackson Lane)
                                           "JA", "JB", "JC", "NB", # Jones Rd N (Baltimore Corner)
                                           "OB", "XB", "MB", "HB", "TP", # Baltimore Corner
                                           "TI", "QB", "DF", # Jones Rd S (Baltimore Corner)
                                           "CR", "TR", "AG")) # Rivers, own name for watershed

# Subset data

SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  filter(Site_dry == "No") %>%
  filter(!(Site == "AG") & !(Site == "TR"))

GW <- filter(merge, Sample_Type == "UW")

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

# Theme stuff
theme <- theme_bw() +
  theme(text = element_text(size = 20), legend.position = "none")

#### Plotting timeseries ####

spring_color <- c("#B0E57C")
summer_color <- c("#FFC107")
fall_color <- c("#8D6E63")
winter_color <- c("#64B5F6")

a2 <- ggplot(SW, aes(x= Date_corrected, y = CO2_uM)) +
  geom_rect(aes(xmin = as.Date("2020-09-01"), xmax = as.Date("2020-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2020-12-01"), xmax = as.Date("2021-03-01"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_rect(aes(xmin = as.Date("2021-03-01"), xmax = as.Date("2021-06-01"), ymin = -Inf, ymax = Inf), fill = spring_color, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2021-06-01"), xmax = as.Date("2021-09-01"), ymin = -Inf, ymax = Inf), fill = summer_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2021-09-01"), xmax = as.Date("2021-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2021-12-01"), xmax = as.Date("2022-03-01"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_rect(aes(xmin = as.Date("2022-03-01"), xmax = as.Date("2022-06-01"), ymin = -Inf, ymax = Inf), fill = spring_color, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2022-06-01"), xmax = as.Date("2022-09-01"), ymin = -Inf, ymax = Inf), fill = summer_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2022-09-01"), xmax = as.Date("2022-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2022-12-01"), xmax = as.Date("2023-03-01"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_point(size=2) +
  labs(x = "Date", y = CO2_lab, tag = "b)") +
  geom_smooth() +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month", expand = c(0,0))
  lims <- as.POSIXct(strptime(c("2020-10-30 00:00", "2022-12-13 00:00"), 
                              format = "%Y-%m-%d %H:%M"))

a2

a3 <- ggplot(SW, aes(x= Date_corrected, y = CH4_uM)) +
  geom_rect(aes(xmin = as.Date("2020-10-30"), xmax = as.Date("2020-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2020-12-01"), xmax = as.Date("2021-03-01"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_rect(aes(xmin = as.Date("2021-03-01"), xmax = as.Date("2021-06-01"), ymin = -Inf, ymax = Inf), fill = spring_color, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2021-06-01"), xmax = as.Date("2021-09-01"), ymin = -Inf, ymax = Inf), fill = summer_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2021-09-01"), xmax = as.Date("2021-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2021-12-01"), xmax = as.Date("2022-03-01"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_rect(aes(xmin = as.Date("2022-03-01"), xmax = as.Date("2022-06-01"), ymin = -Inf, ymax = Inf), fill = spring_color, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2022-06-01"), xmax = as.Date("2022-09-01"), ymin = -Inf, ymax = Inf), fill = summer_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2022-09-01"), xmax = as.Date("2022-12-01"), ymin = -Inf, ymax = Inf), fill = fall_color, alpha = 0.005) +
  geom_rect(aes(xmin = as.Date("2022-12-01"), xmax = as.Date("2022-12-13"), ymin = -Inf, ymax = Inf), fill = winter_color, alpha = 0.008) +
  geom_point(size=2) +
  labs(x = "Date", y = CH4_lab, tag = "c)") +
  geom_smooth() +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month", expand = c(0,0))

a3

b <- plot_grid(wl_graph, a2, a3, ncol = 1, align = "v", rel_heights = c(1, 1, 1))
b

ggsave("Graphs/MS/time series w colors.jpg", b, width = 18, height = 10)

# No data within our sampling period :(

ggplot(precip, aes(x= Date_corrected, y = precip_mm)) +
  geom_col() +
  labs(x = "Date", y = "Precipitation (mm)", tag = "a)") +
  theme +
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month")

t1 <- ggplot(SW, aes(x= Date_corrected, y = CO2_uM)) +
  geom_point(size = 3) +
  labs(x = "", y = CO2_lab, tag = "b)") +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  geom_smooth(method = "loess") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month")

t1

t2 <- ggplot(SW, aes(x= Date_corrected, y = CH4_uM)) +
  geom_point(size = 3) +
  labs(x = "", y = CH4_lab, tag = "c)") +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  geom_smooth(method = "loess") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month")

t <- plot_grid(wl_graph, t1, t2, ncol = 1, align = "v", rel_heights = c(1, 1, 1))
t

ggsave("Graphs/MS/time series.jpg", t, width = 14, height = 10)

ggplot(SW, aes(x= Date_corrected, y = CH4_CO2)) +
  geom_point(size=3) +
  geom_smooth(method = "loess") +
  labs(x = "Sampling date", y = "CH4:CO2 molar ratio") +
  theme +
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "1 month", expand = c(0,0))

ggsave("Graphs/CH4_CO2 timeseries.jpg", width = 28, height = 8, units = "in", dpi = 300)


DO <- ggplot(SW, aes(x= Date_corrected, y = DO_mgL)) +
  geom_point(size = 3) +
  labs(x = "", y = "DO (mg/L)", tag = "a)") +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  geom_smooth(method = "loess") +
  scale_x_date(labels= date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month")

DO

ggsave("DO timeseries.jpg")

do_avg <- SW %>%
  group_by(yymm_new = yymm_new) %>%
  summarise(Mean = mean(DO_mgL), 
            Min = min(DO_mgL),
            Max = max(DO_mgL),
            Date_corrected = first(Date_corrected))


CH4_CO2 <- ggplot(SW, aes(x= Date_corrected, y = CH4_CO2)) +
  geom_point(size = 3) +
  labs(x = "Date", y = "CH4:CO2", tag = "b)") +
  theme_bw() +
  theme +
  theme(legend.position = "none") +
  geom_smooth(method = "loess") +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months", date_minor_breaks = "3 month")

CH4_CO2


redox_avg <- SW %>%
  group_by(yymm_new = yymm_new) %>%
  summarise(Mean = mean(CH4_CO2), Date_corrected = first(Date_corrected),
            max = max(CH4_CO2))


a <- plot_grid(DO, CH4_CO2, ncol = 1, align = "v", rel_heights = c(1, 1, 1))
a

ggsave("Graphs/MS/wl_redox.jpg", a, width = 14, height = 6, units = "in", dpi = 300)


ggplot(SW, aes(x= DO_mgL, y = CH4_CO2)) +
  geom_point(size=3) +
  geom_smooth() +
  theme




# Boxplots of CO2 and CH4 by sampling date, with sites by color and with legend

ggplot(SW, aes(x= yymm_new, y = CO2_uM)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Sampling date", y = CO2_lab) +
  geom_hline(yintercept = 20, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0, aes(color = Site)) +
  theme_bw() +
  theme +
  theme(legend.position = "right")

ggsave("Graphs/Time-series/CO2 by sampling date and site_legend.jpg")

ggplot(SW, aes(x= yymm_new, y = CH4_uM)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Sampling date", y = CH4_lab) +
  geom_hline(yintercept = .003, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0.1, aes(color = Site)) +
  theme_bw() +
  theme +
  scale_y_log10() +
  theme(legend.position = "right")

ggsave("Graphs/Time-series/CH4 by sampling date and site_legend.jpg")


# Boxplots by sampling date without sites or legend

a1 <- ggplot(SW, aes(x= yymm_new, y = CO2_uM)) +
  geom_boxplot(fill = "gray", outlier.alpha = 0.1) +
  labs(x = "", y = CO2_lab, tag = "b)") +
  geom_hline(yintercept = 20, size = 2, alpha = 0.6) + # atm equilibrium line
  geom_jitter(size = 3, width = 0) +
  theme

a1

ggsave("Graphs/Time-series/CO2 by sampling date.jpg")

a2 <- ggplot(SW, aes(x= yymm_new, y = CH4_uM)) +
  geom_boxplot(fill = "gray", outlier.alpha = 0.1) +
  labs(x = "", y = CH4_lab, tag = "c)") +
  geom_smooth() +
  geom_hline(yintercept = 0.003, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0) +
  theme_bw() +
  scale_y_log10(labels = label_comma(accuracy = 0.01)) +
  theme +
  theme(axis.title.x=element_blank())
a2

ggsave("Graphs/Time-series/Ch4 by sampling date.jpg")


## Need to figure out a way to keep x axes continuous so it goes 
# along well with water level. BUT this makes the boxplot look like shit


ggsave("Graphs/Time-series/CH4 by sampling date.jpg")

# p3 <- ggplot(SW, aes(x= yymm_new, y = DO_percent)) +
#   geom_boxplot(fill = "gray") +
#   geom_jitter(size = 3, width = 0) +
#   labs(x = "Sampling date", y = "DO (%)") +
#   geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
#   theme +
#   theme(axis.title.x=element_blank())
# p3
# 
# ggsave("Graphs/Time-series/DO by sampling date.jpg")

# grid.arrange(grobs = list(p1, p2, p3), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 3)))
# grid.arrange(grobs = list(p1, p2, p3), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 3)))

a <- plot_grid(wl_graph, a2, a3, ncol = 1, align = "v", rel_heights = c(1, 1, 1))
a
ggsave("Graphs/MS/CO2_CH4_wl_v3.jpg", a, width = 14, height = 8, units = "in", dpi = 300)


p1 <- ggplot(SW, aes(x= yymm_new, y = DO_percent)) +
  geom_boxplot(fill = "gray") +
  geom_jitter(size = 3, width = 0) +
  labs(x = "Sampling date", y = "DO (%)", tag = "d") +
  geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
  theme +
  theme(axis.title.x=element_blank())
p1

ggsave("Graphs/Time-series/DO by sampling date.jpg")

SW$Temp_C <- as.numeric(SW$Temp_C)
p2 <- ggplot(SW, aes(x= yymm_new, y = Temp_C)) +
  geom_boxplot(fill = "gray") +
  geom_jitter(size = 3, width = 0) +
  labs(x = "Sampling date", y = expression("Temp " ( degree*C)), tag = "e") +
  theme +
  theme(axis.title.x=element_blank())
p2


SW <- filter(SW, NPOC < 120)

p3 <- ggplot(SW, aes(x= yymm_new, y = NPOC)) +
  geom_boxplot(fill = "gray") +
  geom_jitter(size = 3, width = 0) +
  labs(x = "Sampling date", y = "DOC (mg/L)", tag = "f") +
  theme +
  theme(axis.title.x=element_blank())
p3

ggsave("Graphs/Time-series/DO by sampling date.jpg")

# grid.arrange(grobs = list(p1, p2, p3), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 3)))
# grid.arrange(grobs = list(p1, p2, p3), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 3)))

b <- plot_grid(p1, p2, p3, ncol = 1, align = "v", rel_heights = c(1, 1, 1))
b
ggsave("Graphs/MS/CO2_CH4_wl.jpg", g)

c <- plot_grid(a, b)
ggsave("Graphs/MS/Figure 6_new.jpg", c, width = 22, height = 10, dpi = 150, units = "in")



# Plotting percent saturation
# From Percent_saturation script

ggplot(SW, aes(x= yymm_new, y = CO2_sat_per)) +
  geom_boxplot(fill = "gray", position = position_dodge(1)) +
  labs(x = "", y = "CO2 saturation (%)") +
  geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0) +
  theme_bw() + theme(axis.ticks.x=element_blank()) + theme(axis.text.x=element_blank()) +
  theme

ggsave("Graphs/Time-series/CO2_sat.jpg")

ggplot(SW, aes(x= yymm_new, y = CH4_sat_per)) +
  geom_boxplot(fill = "gray") +
  labs(x = "Sampling date", y = CH4_lab) +
  geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0.1) +
  theme_bw() +
  scale_y_log10() +
  theme

ggsave("Graphs/Time-series/CH4_sat.jpg")
