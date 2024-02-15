# Path analysis

library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)

# Read in merged
merge <- read.csv("Master spreadsheet.csv")

# Fix date
merge$Date_corrected <- as.Date(parse_date_time(merge$Date_corrected, c("mdy", "ymd")))

# Subset data

SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  filter(Site_dry == "No") %>%
  filter(!(Site == "AG") & !(Site == "TR"))

GW <- filter(merge, Sample_Type == "UW")

# Define model

model_CO2 <- "CO2_uM ~ pH + DO_percent + CH4_uM + SpC + Temp_C + NPOC + d18O + TDN + TDP + SO4 + NO3"

fit_CO2 <- cfa(model_CO2, data = SW)

summary(fit_CO2, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit_CO2, 'std', layout = 'circle')
semPaths(fit_CO2,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

model_CH4 <- "CH4_uM ~ pH + DO_percent + CO2_uM + SpC + Temp_C + NPOC +
                  d18O + TDN"

fit_CH4 <- cfa(model_CH4, data = SW)

summary(fit_CH4, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit_CH4, 'std', layout = 'circle')
semPaths(fit_CH4,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

