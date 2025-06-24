# Plotting merged data set
# This scripts has:
  # 1) Boxplots for SW-GW
# Carla Lopez Lloreda
# Updated 10/31/2023

library(dplyr)
library(ggplot2)
library(methods) # for geom_stat
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr) # for p values and r-squared on graph
library(lubridate)
library(gridExtra) # for merging plots
library(scales)
library(plotly)

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

#### Cleaning and merging data ####

# Read in merged (includes all analytes, GHG, field data, water level, and watershed characteristics)
merge <- read.csv("Master spreadsheet.csv")

# Subset data

# Remove rivers, remove channels
merge <- merge %>%
  filter(!(Site == "AG") & !(Site == "TR") & !(Site == "CR")) %>%
  filter(Site_dry == "No") %>%
  filter(Sample_Type == "SW" | Sample_Type == "UW") %>%
  filter(CH4_uM > 0)

SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  filter(Site_dry == "No") %>%
  filter(!(Site == "AG") & !(Site == "TR"))

GW <- filter(merge, Sample_Type == "UW")

#### Plots ####

# Theme stuff
theme <- theme_bw() +
  theme(text = element_text(size = 20))
# theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                    axis.line = element_line(colour = "black"))

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

CO2_log_lab <- expression(paste("log10(C","O"[2]^{}*" ("*mu,"M))"))
CH4_log_lab <- expression(paste("log10(C","H"[4]^{}*" ("*mu,"M))"))

#### Surface water vs groundwater ####

# Sample type vs GHG w/ Tukey test: only for SW and GW

ANOVA_CO2 <- aov(merge$CO2_uM~merge$Sample_Type)
TUKEY_CO2 <- TukeyHSD(ANOVA_CO2)
cld_CO2 <- multcompLetters4(ANOVA_CO2, TUKEY_CO2)
dt_CO2 <- merge %>% 
  group_by(Sample_Type) %>%
  summarise(w = mean(CO2_uM), sd = sd(CO2_uM)) %>%
  arrange(desc(w))
cld_CO2 <- as.data.frame.list(cld_CO2$`merge$Sample_Type`)
dt_CO2$cld <- cld_CO2$Letters
dt_CO2$CO2_uM <- dt_CO2$w

summary(ANOVA_CO2)

p1 <- ggplot(NULL, aes(x = Sample_Type, y = CO2_uM, shape = Sample_Type, color = Sample_Type)) + 
  geom_boxplot(data = merge) +
  geom_jitter(data = merge, width = 0.1, size = 3) +
  labs(x = "", y = CO2_lab, tag = "a)") +
  geom_label(data = dt_CO2, aes(x = Sample_Type, y = w + sd + 1300, label = cld), size = 8, alpha = 1) +
  theme +
  theme(legend.position = "none") + 
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland surface water", "UW" = " Wetland groundwater")) +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none")

p1

ggsave("Graphs/Boxplots/CO2 vs sample type_clean.jpg")

ANOVA_CH4 <- aov(merge$CH4_uM~merge$Sample_Type)
TUKEY_CH4 <- TukeyHSD(ANOVA_CH4)
cld_CH4 <- multcompLetters4(ANOVA_CH4, TUKEY_CH4)
dt_CH4 <- merge %>% 
  group_by(Sample_Type) %>%
  summarise(w = mean(CH4_uM), sd = sd(CH4_uM)) %>%
  arrange(desc(w))
cld_CH4 <- as.data.frame.list(cld_CH4$`merge$Sample_Type`)
dt_CH4$cld <- cld_CH4$Letters
dt_CH4$CH4_uM <- dt_CH4$w

summary(ANOVA_CH4)

p2 <- ggplot(NULL, aes(x = Sample_Type, y = CH4_uM, shape = Sample_Type, color = Sample_Type)) + 
  geom_boxplot(data = merge) +
  geom_jitter(data = merge, width = 0.1, size = 3) +
  labs(x = "", y = CH4_lab, tag = "b)") +
  geom_label(data = dt_CH4, aes(x = Sample_Type, y = w + sd + 250, label = cld), size = 8, alpha = 1) +
  theme +
  theme(legend.position = "none") + 
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland surface water", "UW" = " Wetland groundwater")) +
  scale_y_log10(labels = scales::comma) +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none")

  
p2

ggsave("Graphs/Boxplots/CH4 vs sample type_clean.jpg")

# Run stats on boxplots: t-test

t.test(CO2_uM ~ Sample_Type, data = merge)

        # Welch Two Sample t-test
        # 
        # data:  CO2_uM by Sample_Type
        # t = -12.586, df = 133.68, p-value < 2.2e-16
        # alternative hypothesis: true difference in means between group SW and group UW is not equal to 0
        # 95 percent confidence interval:
        #   -814.5987 -593.3407
        # sample estimates:
        #   mean in group SW mean in group UW 
        # 341.1149        1045.0846

t.test(CH4_uM ~ Sample_Type, data = merge)

        # Welch Two Sample t-test
        # 
        # data:  CH4_uM by Sample_Type
        # t = 3.168, df = 243.89, p-value = 0.001731
        # alternative hypothesis: true difference in means between group SW and group UW is not equal to 0
        # 95 percent confidence interval:
        #   2.296927 9.848528
        # sample estimates:
        #   mean in group SW mean in group UW 
        # 12.202111         6.129384

# CH4 vs CO2
p3 <- ggplot(merge, aes(x = CO2_uM, y = CH4_uM, color = Sample_Type)) +
  geom_point(size = 4, aes(shape= Sample_Type)) +
  geom_smooth(method = "lm", aes(shape= Sample_Type)) +
  labs(x = CO2_lab, y = CH4_lab, tag = "c)") +
  theme_bw() +
  scale_y_continuous(trans='log10',
                     labels = function(x)round(x,2)
  ) +
  theme +
  theme(legend.position = "none")

p3

summary(lm(CH4_uM~CO2_uM, SW))
summary(lm(CH4_uM~CO2_uM, GW))

ggsave("Graphs/Regressions/CH4 vs CO2_all.jpg")


p <- grid.arrange(p1, p2, p3, ncol = 2, 
             layout_matrix = cbind(c(1,1,2,2), c(3,3,3)))

ggsave("Graphs/MS/Fig 2.jpg", p, width = 14, height = 6, dpi = 300)

#### Across all samples types ####

# All sample types vs CO2
ggplot(merge, aes(x = Sample_Type, y = CO2_uM)) +
  geom_boxplot() +
  labs(x = "Sample type", y = CO2_lab) + 
  scale_x_discrete(labels=c("CH" = "Channel", "RI" = "River", 
                            "SW" = "Wetland Surface water", "UW" = " Wetland Groundwater")) +
  theme(axis.text.x = element_text(angle=25), axis.title = element_text(size=15)) +
  geom_jitter()

ggsave("Graphs/CO2 vs sample type.jpg")


# All samples types vs CH4
ggplot(merge, aes(x = Sample_Type, y = CH4_uM)) +
  geom_boxplot() +
  labs(x = "Sample type", y = CH4_lab) +
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland Surface water", "UW" = " Wetland Groundwater")) +
  theme(axis.text.x = element_text(angle=25), axis.title = element_text(size=15)) +
  geom_jitter()

ggsave("Graphs/CH4 vs sample type.jpg")


