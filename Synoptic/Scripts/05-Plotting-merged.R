# Plotting merged data set
# This scripts has:
  # 1) Boxplots for SW-GW
  # 2) Boxplots for GHG across sites
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

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

#### Cleaning and merging data ####

# Read in merged (includes all analytes, GHG, field data, and watershed characteristics)
merge <- read.csv("Master spreadsheet.csv")

# Rearrange factor order by site/watershed

merge$Site <- factor(merge$Site,levels = c("ND", "BD", "TS", "DK", "FR", # Jackson Lane
                                               "TA", "TB", "DB", "FN", # Tiger Paw/Beetree Rd (Jackson Lane)
                                               "JA", "JB", "JC", "NB", # Jones Rd N (Baltimore Corner)
                                               "OB", "XB", "MB", "HB", "TP", # Baltimore Corner
                                               "TI", "QB", "DF", # Jones Rd S (Baltimore Corner)
                                               "CR", "TR", "AG")) # Rivers, own name for watershed

merge <- filter(merge, Sample_Type == "SW" | Sample_Type == "UW")
merge <- filter(merge, !Site == "CR")
merge <- filter(merge, CH4_uM > 0)
merge <- filter(merge, !Field_flag == 1)

merge$watershed <- ifelse(merge$Site == "ND" | merge$Site == "BD" |
                              merge$Site == "TS" | merge$Site == "DK" | merge$Site == "FR", merge$watershed <- "Jackson Lane", 
                            ifelse(merge$Site == "TA" | merge$Site == "TB" | merge$Site == "DB" |
                                     merge$Site == "FN", merge$watershed <- "Tiger Paw",
                                   ifelse(merge$Site == "JA" | merge$Site == "JB" |
                                            merge$Site == "JC" | merge$Site == "NB", merge$watershed <- "Jones Road" , 
                                          ifelse(merge$Site == "OB" | merge$Site == "XB" |
                                                   merge$Site == "MB" | merge$Site == "HB", merge$watershed <- "Baltimore Corner", 
                                                 ifelse(merge$Site == "TI" | merge$Site == "QB" | merge$Site == "DF", merge$watershed <- "Baltimore Corner", NA)))))

# Subset data

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


#### Surface water vs groundwater ####

# Sample type vs GHG: only for SW and GW

ggplot(merge, aes(x = Sample_Type, y = CO2_uM, shape = Sample_Type, color = Sample_Type)) +
  geom_boxplot() +
  labs(x = "", y = CO2_log_lab, tag = "a)") + 
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland surface water", "UW" = " Wetland groundwater")) +
  theme +
  theme(legend.position = "none") +
  geom_jitter(size = 4, width = 0.1) + 
  scale_y_log10()


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

ggplot(merge, aes(x = Sample_Type, y = CH4_uM, shape = Sample_Type, color = Sample_Type)) +
  geom_boxplot() +
  labs(x = "", y = CH4_log_lab, tag = "b)") +
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland surface water", "UW" = " Wetland groundwater")) +
  theme +
  theme(legend.position = "none") +
  geom_jitter(size = 4, width = 0.1) +
  scale_y_log10(labels = label_comma(accuracy = 0.1))


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
  labs(x = "", y = CH4_log_lab, tag = "b)") +
  geom_label(data = dt_CH4, aes(x = Sample_Type, y = w + sd + 250, label = cld), size = 8, alpha = 1) +
  theme +
  theme(legend.position = "none") + 
  scale_x_discrete(labels=c("CH" = "Channel well", "RI" = "River", 
                            "SW" = "Wetland surface water", "UW" = " Wetland groundwater")) +
  scale_y_log10() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none")

  
p2

ggsave("Graphs/Boxplots/CH4 vs sample type_clean.jpg")

# Run stats on boxplots: t-test

t.test(CO2_uM ~ Sample_Type, data = merge)

        # Welch Two Sample t-test
        # 
        # data:  CO2_uM by Sample_Type
        # t = -10.699, df = 105.34, p-value < 2.2e-16
        # alternative hypothesis: true difference in means is not equal to 0
        # 95 percent confidence interval:
        #   -793.6752 -545.5060
        # sample estimates:
        #   mean in group SW mean in group UW 
        # 334.6024        1004.1930 

t.test(CH4_uM ~ Sample_Type, data = merge)

        # Welch Two Sample t-test
        #
        # data:  CH4_uM by Sample_Type
        # t = 3.3473, df = 160.49, p-value = 0.001017
        # alternative hypothesis: true difference in means is not equal to 0
        # 95 percent confidence interval:
        #   3.144285 12.193026
        # sample estimates:
        #   mean in group SW mean in group UW 
        # 13.252150         5.583495

# CH4 vs CO2
p3 <- ggplot(merge, aes(x = CO2_uM, y = CH4_uM, color = Sample_Type)) +
  geom_point(size = 4, aes(shape= Sample_Type)) +
  geom_smooth(method = "lm", aes(shape= Sample_Type)) +
  labs(x = CO2_lab, y = CH4_lab, tag = "c)") +
  theme_bw() +
  theme +
  theme(legend.position = "none")

p3

summary(lm(CH4_uM~CO2_uM, SW))
summary(lm(CH4_uM~CO2_uM, GW))

ggsave("Graphs/Regressions/CH4 vs CO2_all.jpg")


p <- grid.arrange(p1, p2, p3, ncol = 2, 
             layout_matrix = cbind(c(1,1,2,2), c(3,3,3)))

ggsave("Graphs/MS/Figure 2.jpg", p, width = 14, height = 6, dpi = 300)


#### Boxplots across sites ####

p1 <- ggplot(SW, aes(x = Site, y = CO2_uM, fill = wetland_complex)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, fill = "Wetland complex", tag = "a") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=18), axis.text = element_text(size = 16)) +
  theme(
    legend.position = c(.90, .30),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  ) +
  theme +
  scale_y_log10()
p1

ggsave("Graphs/Boxplots/CO2_SW_boxplot.jpg")

p2 <- ggplot(SW, aes(x = Site, y = CH4_uM, fill = wetland_complex)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme +
  scale_y_log10()
  # theme(
  #   legend.position = c(.99, .99),
  #   legend.justification = c("right", "top"),
  #   legend.box.just = "right",
  #   legend.margin = margin(6, 6, 6, 6),
  #   axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  # )
p2

ggsave("Graphs/Boxplots/CH4_SW_boxplot.jpg")

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3.jpg", g)



## Grouping by "watersheds"
## Groups are: Jackson Lane, Beetree Road, Baltimore Corner, Jones Road

p1 <- ggplot(SW, aes(x = Site, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, fill = "Wetland complex", tag = "a)") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=18), axis.text = element_text(size = 16)) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  ) +
  theme +
  scale_y_log10()
p1

ggsave("Graphs/Boxplots/CO2_SW_boxplot.jpg")

p2 <- ggplot(SW, aes(x = Site, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme
# theme(
#   legend.position = c(.99, .99),
#   legend.justification = c("right", "top"),
#   legend.box.just = "right",
#   legend.margin = margin(6, 6, 6, 6),
#   axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
# )

p2

ggsave("Graphs/Boxplots/CH4_SW_boxplot.jpg")

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3 alternative.jpg", g)



# Tukey test on CO2 across sites
ANOVA_CO2 <- aov(SW$CO2_uM~SW$Site)
summary(ANOVA_CO2)
TUKEY_CO2 <- TukeyHSD(ANOVA_CO2)
cld_CO2 <- multcompLetters4(ANOVA_CO2, TUKEY_CO2)
dt_CO2 <- SW %>% 
  group_by(Site) %>%
  summarise(w = mean(CO2_uM), sd = sd(CO2_uM)) %>%
  arrange(desc(w))
cld_CO2 <- as.data.frame.list(cld_CO2$`SW$Site`)
dt_CO2$cld <- cld_CO2$Letters
dt_CO2$CO2_uM <- dt_CO2$w

# ANOVA for CO2
summary(ANOVA_CO2)
print(TUKEY_CO2)

# CO2 across sites with Tukey

p1 <- ggplot(NULL, aes(x = reorder(Site, -CO2_uM, mean), y = CO2_uM)) + 
  geom_boxplot(data = SW, aes(x = reorder(Site, -CO2_uM, mean), y = CO2_uM, fill = watershed)) + 
  geom_jitter(data = SW, width = 0.1) +
  labs(x = "Site", y = CO2_lab, tag = "a)", fill = "Wetland complex") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  geom_label(data = dt_CO2, aes(x = Site, y = w + sd + 150, label = cld), size = 6, alpha = 1)

p1


# Tukey test on CH4 across sites

ANOVA_CH4 <- aov(SW$CH4_uM~SW$Site)
TUKEY_CH4 <- TukeyHSD(ANOVA_CH4)
cld_CH4 <- multcompLetters4(ANOVA_CH4, TUKEY_CH4)
dt_CH4 <- SW %>% 
    group_by(Site) %>%
    summarise(w = mean(CH4_uM), sd = sd(CH4_uM)) %>%
    arrange(desc(w))
cld_CH4 <- as.data.frame.list(cld_CH4$`SW$Site`)
dt_CH4$cld <- cld_CH4$Letters
dt_CH4$CH4_uM <- dt_CH4$w

summary(ANOVA_CH4)
print(TUKEY_CH4)


# ggplot(NULL, aes(x = reorder(Site, -CH4_uM, mean), y = CH4_uM)) + 
#   geom_boxplot(data = SW, aes(fill = wetland_complex)) + 
#   geom_jitter(data = SW) +
#   geom_text(data = dt, aes(label = cld, y = w + sd + 5), vjust = -0.5, colour = 'white') +
#   labs(x = "Site", y = CH4_lab, tag = "b)") +
#   theme

p2 <- ggplot(NULL, aes(x = reorder(Site, -CH4_uM, mean), y = CH4_uM)) + 
  geom_boxplot(data = SW, aes(x = reorder(Site, -CH4_uM, mean), y = CH4_uM, fill = watershed)) + 
  geom_jitter(data = SW, width = 0.1) + 
  labs(x = "Site", y = CH4_log_lab, tag = "b)", fill = "Wetland complex") +
  scale_y_log10() +
  geom_label(data = dt_CH4, aes(x = Site, y = w + sd + 100, label = cld), size = 6, alpha= 1)  +
  theme +
  theme(axis.text.x = element_text(size = "20"), axis.title = element_text(size = 20), legend.position = "none")

p2

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3_Tukey.jpg", g, width = 14, height = 8, units = "in", dpi = 300)

# Plotting across wetland complexes

ANOVA_CO2_complex <- aov(SW$CO2_uM~SW$watershed)
TUKEY_CO2_complex <- TukeyHSD(ANOVA_CO2_complex)
cld_CO2_complex <- multcompLetters4(ANOVA_CO2_complex, TUKEY_CO2_complex)
dt_CO2_complex <- SW %>% 
  group_by(watershed) %>%
  summarise(w = mean(CO2_uM), sd = sd(CO2_uM)) %>%
  arrange(desc(w))
cld_CO2_complex <- as.data.frame.list(cld_CO2_complex$`SW$Site`)
dt_CO2_complex$cld <- cld_CO2_complex$Letters
dt_CO2_complex$CO2_uM <- dt_CO2_complex$w

summary(ANOVA_CO2_complex)
print(TUKEY_CO2_complex)


c1 <- ggplot(SW, aes(x = watershed, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, tag = "a)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme



ANOVA_CH4_complex <- aov(SW$CH4_uM~SW$watershed)
TUKEY_CH4_complex <- TukeyHSD(ANOVA_CH4_complex)
cld_CH4_complex <- multcompLetters4(ANOVA_CH4_complex, TUKEY_CH4_complex)
dt_CH4_complex <- SW %>% 
  group_by(watershed) %>%
  summarise(w = mean(CH4_uM), sd = sd(CH4_uM)) %>%
  arrange(desc(w))
cld_CH4_complex <- as.data.frame.list(cld_CH4_complex$`SW$Site`)
dt_CH4_complex$cld <- cld_CH4_complex$Letters
dt_CH4_complex$CH4_uM <- dt_CH4_complex$w

summary(ANOVA_CH4_complex)
print(TUKEY_CH4_complex)


c2 <- ggplot(SW, aes(x = watershed, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme

c <- plot_grid(c1, c2, ncol= 1)
c

ggsave("Graphs/MS/wetland complex.jpg")


# For groundwater

ggplot(UW, aes(x = Site, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Site", y = CH4_lab) +
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
  labs(x = "Site", y = CO2_lab) +
  theme(
    legend.position = c(.86, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CO2_UW_boxplot.jpg")




# Creating well number column

all_UW$well <- as.numeric(substr(all_UW [ , 1], start= 9, stop= 9))

# Looking at CO2-DOC residuals

CO2_DOC <- lm(CO2_uM ~ NPOC_mgC_L, data = all_UW)
ols_plot_resid_fit(CO2_DOC)

all_UW$residuals <- CO2_DOC$residuals

ggplot(all_UW, aes(x = Site_ID, y = residuals)) +
  geom_point() +
  labs(x = "Well position", y = "Residuals") +
  theme

ggsave("Graphs/residuals CO2 vs DOC_all.jpg")


# This is pretty cool:
ggstatsplot::ggscatterstats(data = SW, x = NPOC, y = CO2_uM)
