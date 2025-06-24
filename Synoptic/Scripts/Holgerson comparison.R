library(readr)
library(plotrix)

holgerson <- read_csv("Holgerson small wetlands data.csv")

holgerson$co2_umolL <- as.numeric(holgerson$co2_umolL)
holgerson$ch4_umolL <- as.numeric(holgerson$ch4_umolL)

mean(holgerson$co2_umolL, na.rm = TRUE)
std.error(holgerson$co2_umolL, na.rm = TRUE)
sd(holgerson$co2_umolL, na.rm = TRUE)

dmv <- read_csv("GHG stats summary w area.csv")


mean(dmv$avg_CO2, na.rm = TRUE)
std.error(dmv$avg_CO2, na.rm = TRUE)
sd(dmv$avg_CO2, na.rm = TRUE)
