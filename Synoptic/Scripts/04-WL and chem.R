library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)

# Plotting water level and chem

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data

merge <- read.csv("Master spreadsheet.csv")

merge <- filter(merge, !Sample_Type == "RI" & !Sample_Type == "CH")

merge <- filter(merge, !Field_flag == 1)

# Filter for SW

SW <- filter(merge, Sample_Type == "SW")
UW <- filter(merge, Sample_Type == "UW")

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

# Theme stuff
theme <- theme_bw() +
  theme(text = element_text(size = 20), legend.position = "none")

ggplot(SW, aes(x= Temp_C, y = CO2_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  theme

ggscatterstats(SW, CH4_uM, CO2_uM)

ggscatterstats(SW, CH4_uM, CO2_uM)


ggplot(SW, aes(x= CH4_uM, y = CO2_uM, color = Site)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, se = F) +
  theme

ggplot(SW, aes(x= Temp_C, y = CH4_uM, color = DO_mgL)) +
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  theme

summary(lm(CO2_uM~Temp_C, SW))




ggplot(SW, aes(x= DO_mgL, y = CO2_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, se= F) +
  scale_y_log10() +
  theme +
  labs(x= "Dissolved oxygen (mg/L)", y = CO2_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 6, size = 5, p.accuracy = 0.001)

ggplot(SW, aes(x= DO_mgL, y = CO2_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, se= F) +
  scale_y_log10() +
  theme +
  labs(x= "Dissolved oxygen (mg/L)", y = CO2_lab)

summary(lm(CO2_uM~DO_mgL, SW))

ggsave("CO2 vs DO.jpg")



ggplot(SW, aes(x= DO_mgL, y = CH4_uM)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, se= F) +
  theme +
  labs(x= "Dissolved oxygen (mg/L)", y = CH4_lab)

summary(lm(CH4_uM~DO_mgL, SW))

ggsave("CH4 vs DO.jpg")





# Plot relationships with water level

s1 <- ggplot(SW, aes(x= dly_mean_wtrlvl, y = CO2_uM, color = Sample_Type)) +
  geom_vline(xintercept= 0, size = 1) +
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  labs(x = "", y = CO2_lab, tag = "a)") +
  theme

summary(lm(CO2_uM~dly_mean_wtrlvl, SW))

s1

ggsave("Graphs/SW CO2 vs daily wl_linear.jpg")


# Trying a polynomial relationship for CO2 vs WL
SW$dly_mean_wtrlvl <- as.data.frame(na.omit(SW$dly_mean_wtrlvl))
SW_new <- as.data.frame(SW$dly_mean_wtrlvl[!is.na(SW$dly_mean_wtrlvl)])

fit <- lm(formula = CO2_uM ~ poly(dly_mean_wtrlvl, 2), data = SW_new)

summary(fit)

ggplot(wetland_info, aes(area_m2, p_a_ratio)) +
  geom_point() +
  geom_smooth(aes(area_m2, predict(fit))) +
  labs(x = "Area (m^2)", y = "Perimeter:area ratio") +
  theme

s2 <- ggplot(SW, aes(x= dly_mean_wtrlvl, y = CH4_uM, color = Sample_Type)) +
  geom_point(size = 3) +
  geom_vline(xintercept= 0, size = 1) +
  labs(x = "Daily mean water level (m)", y = CH4_lab, tag = "b)") +
  theme

summary(lm(CH4_uM~dly_mean_wtrlvl, SW))

s2

ggsave("Graphs/SW CH4 vs daily wl.jpg")

s <- plot_grid(s1, s2, ncol= 1)
s

ggsave("Graphs/MS/SW_GHG and wl.jpg", dpi = 300, width = 9, height = 7)

g1 <- ggplot(UW, aes(x= dly_mean_wtrlvl, y = CO2_uM)) +
  geom_point(size = 3, color = "#00BFC4") +
  geom_vline(xintercept= 0, size = 1) +
  labs(x = "Daily mean water level (m)", y = CO2_lab, tag = "a)") +
  theme

g1

summary(lm(CO2_uM~dly_mean_wtrlvl, UW))

g2 <- ggplot(UW, aes(x= dly_mean_wtrlvl, y = CH4_uM)) +
  geom_point(size = 3, color = "#00BFC4") +
  geom_vline(xintercept= 0, size = 1) +
  labs(x = "Daily mean water level (m)", y = CH4_lab, tag = "c)") +
  theme

summary(lm(CH4_uM~dly_mean_wtrlvl, UW))

g <- plot_grid(g1, g2, ncol = 1)
g

wl_CO2 <- plot_grid(g, s)
wl_CO2

ggsave("Graphs/MS/Daily WL and GHG.jpg", width = 14, height = 6, dpi = 300)


ggplot(SW, aes(x= dly_mean_wtrlvl, y = NPOC)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(merge, aes(x= dly_mean_wtrlvl, y = CO2_uM, color = Sample_Type)) +
  geom_point() +
  geom_vline(xintercept= 0) +
  geom_smooth(method = "lm") +
  labs(x = "Daily mean water level (m)", y = CO2_lab) +
  theme +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0.2, size = 5)

ggsave("Graphs/CO2 vs WL_SW_GW.jpg")


ggplot(merge, aes(x= dly_mean_wtrlvl, y = CH4_uM, color = Sample_Type)) +
  geom_point() +
  geom_vline(xintercept= 0) +
  labs(x = "Daily mean water level (m)", y = CH4_lab) +
  theme

ggsave("Graphs/CH4 vs WL_SW_GW.jpg")


ggplot(SW, aes(x= dly_mean_wtrlvl, y = CH4_CO2)) +
  geom_point(size=3) +
  geom_smooth() + 
  labs(x = "Daily mean water level (m)", y = "CH4:CO2 molar ratio") +
  theme

ggplot(merge, aes(x= dly_mean_wtrlvl, y = d2H, color = Sample_Type)) +
  geom_point()

ggplot(merge, aes(x= Cl, y = SpC, color = Sample_Type)) +
  geom_point()

ggplot(merge, aes(x= d18O, y = d2H, color = Sample_Type)) +
  geom_point()
