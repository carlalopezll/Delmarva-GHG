# Plotting CVs

library(gridExtra)
library(cowplot)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in CV data

CV_site <- read.csv("Site CV_240828.csv")
CV_time_SW <- read.csv("Time CV_SW.csv")
CV_time_GW <- read.csv("Time CV_UW.csv")

# Filter SW and GW

CV_site_SW <- filter(CV_site, Sample_Type == "SW")
CV_site_GW <- filter(CV_site, Sample_Type == "UW")

# Axis titles for subscripts in CH4 and CO2
CO2_CV_lab <- expression(paste("C","O"[2]^{}*" CV (%)"))
CH4_CV_lab <- expression(paste("C","H"[4]^{}*" CV (%)"))

CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

# Theme stuff
theme <- theme_bw() + theme(axis.text = element_text(size = "20"), axis.title = element_text(size=20)) +
  theme(legend.position = "none")

# Plotting site CVs

# Does CH4 and CO2 variability correlate?
p1 <- ggplot(CV_site_SW, aes(x= CV_CO2, y= CV_CH4, color = Site)) +
  geom_point() +
  labs(x = CO2_CV_lab, y = CH4_CV_lab, tag = "a)") +
  theme + 
  geom_label(aes(label=Site)) +
  theme(legend.position = "none") +
  geom_abline(slope = 1) +
  scale_x_log10() +
  xlim(15,125) + ylim(0, 170)

p1

t1 <- ggplot(CV_site_SW, aes(x= CV_CO2, y= CV_CH4, color = Sample_Type)) +
  geom_point(size=3) +
  labs(x = "", y= "", tag = "a)") +
  theme +
  geom_abline(slope = 1) +
  xlim(0,201) + ylim(0,200)

t1

ggsave("Graphs/CVs/CV-CH4 vs CV-CO2_SW_no label.jpg")

summary(lm(formula = CV_CH4~CV_CO2, data = CV_site_SW))

t2 <- ggplot(CV_site_GW, aes(x= CV_CO2, y= CV_CH4)) +
  geom_point(size=3, color = "#00BFC4") +
  labs(x = "", y= "", tag = "b)") +
  theme +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0, 210)

t2

ggsave("Graphs/CVs/CV-CH4 vs CV-CO2_SW_no label.jpg")

t <- plot_grid(t1, t2, ncol = 2)

t

ggsave("Graphs/MS/Site CV.jpg")


ggplot(CV_site_SW, aes(x= CV_CO2, y= CV_CH4, color = CV_wl)) +
  geom_point(size=3) +
  labs(x = CO2_CV_lab, y = CH4_CV_lab, tag = "a)") +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0, 210)


#### CVs across sites ####

ggplot(CV_site_GW, aes(x= CV_CO2, y= CV_CH4, color = Site)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  geom_abline(slope = 1) +
  theme(legend.position = "none")

ggsave("Graphs/CV-CH4 vs CV-CO2_UW.jpg")

ggplot(CV_site_GW, aes(x= CV_CO2, y= CV_CH4)) +
  geom_point(size=3, color = "#00BFC4") +
  labs(x = CO2_CV_lab, y = CH4_CV_lab) +
  theme +
  theme(legend.position = "none") +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0,210)

ggsave("Graphs/CVs/CV-CH4 vs CV-CO2_GW_no label.jpg")



ggplot(CV_site_SW, aes(x = Site, y = CV_CH4)) + # All SW sites
  geom_point()

ggsave("Graphs/CV_SW sites_CH4.jpg")

ggplot(CV_site_SW, aes(x = Site, y = CV_CO2)) +
  geom_point()

ggsave("Graphs/CV_SW sites_CO2.jpg")

ggplot(CV_site_GW, aes(x = Site, y = CV_CH4)) + # All UW sites
  geom_point()

ggsave("Graphs/CV_UW sites_CH4.jpg")

ggplot(CV_site_GW, aes(x = Site, y = CV_CO2)) +
  geom_point()

ggsave("Graphs/CV_UW sites_CO2.jpg")


ggplot(CV_site_SW, aes(x = CV_wl, y = CV_CO2)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  geom_smooth(method = 'lm')

ggplot(CV_site_SW, aes(x = CV_wl, y = CV_CH4)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  geom_smooth(method = 'lm')

#### Plotting time CVs ####

ggplot(CV_time_SW, aes(x= CV_CO2, y= CV_CH4, color = yymm_new)) +
  geom_point() +
  geom_label(aes(label=yymm_new, size = 12)) +
  theme(legend.position = "none")+
  theme +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0,210) +
  labs(x = CO2_CV_lab, y= CH4_CV_lab)

ggsave("Graphs/Time CV-CH4 vs CV-CO2_SW.jpg")

ggplot(CV_time_GW, aes(x= CV_CO2, y= CV_CH4, color = yymm_new)) +
  geom_point() +
  geom_label(aes(label=yymm_new, size = 12)) +
  theme(legend.position = "none")+
  theme +
  geom_abline(slope = 1) +
  
  xlim(0,170) + ylim(0,310) +
  labs(x = CO2_CV_lab, y= CH4_CV_lab)

ggsave("Graphs/Time CV-CH4 vs CV-CO2_GW.jpg")



s1 <- ggplot(CV_time_SW, aes(x= CV_CO2, y= CV_CH4, color = Sample_Type)) +
  geom_point(size = 3) +
  theme(legend.position = "none")+
  theme +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0,200) +
  labs(x = "", y= "", tag = "c)")

s1

ggsave("Graphs/Time CV-CH4 vs CV-CO2_SW.jpg")

s2 <- ggplot(CV_time_GW, aes(x= CV_CO2, y= CV_CH4)) +
  geom_point(size = 3, color = "#00BFC4") +
  theme(legend.position = "none")+
  theme +
  geom_abline(slope = 1) +
  xlim(0,170) + ylim(0,310) +
  labs(x = "", y= "", tag = "d)")

ggsave("Graphs/Time CV-CH4 vs CV-CO2_GW.jpg")

s <- plot_grid(s1, s2, ncol = 2)
s

ggsave("Graphs/MS/Temporal CV.jpg")



cv <- plot_grid(t, s, ncol= 1)
cv

ggsave("Graphs/MS/Fig 3.jpg")


ggplot(CV_time_SW, aes(x= yymm_new, y= CV_CH4)) +
  geom_point(size= 3) +
  theme(legend.position = "none") +
  theme

ggplot(CV_time_SW, aes(x= yymm_new, y= CV_CO2)) +
  geom_point(size=5) +
  geom_smooth(method= lm , color = "black", se=FALSE) +
  theme(legend.position = "none")
