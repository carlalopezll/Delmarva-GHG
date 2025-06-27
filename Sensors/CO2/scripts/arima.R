# Script that uses ARIMA models to look at the influence of precipitation on CO2 in Delmarva wetlands
# Carla López Lloreda

# load libraries
library(tseries)
library(forecast) # for auto.arima()
library(fable) # for ARIMA()
library(tsibble)
library(readr)
library(broom)

# read in high-frequency data
sensors_hf <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# extracting a date column
sensors_hf <- sensors_hf %>%
  mutate(date = as.Date(Timestamp_corrected))

#### Daily means ####
# Suggestion by Cayelan was to use daily means, amplitude range, instead of the high-frequency data

# Looking at a subset of data without gaps
sensors_subset <- sensors_hf %>%
  filter(date > "2021-09-28" & date < "2021-11-14")

# Calculating daily metrics
# should probably just have a separate script that calculates these metrics so I'm not replicating this

# CV = (Standard Deviation / Mean) * 100
sensors_daily <- sensors_hf %>%
  group_by(Site_ID, date) %>%
  summarise(CO2_uatm_mean = mean(CO2_cal_uatm, na.rm = TRUE),
         CO2_uatm_sd = sd(CO2_cal_uatm, na.rm = TRUE),
         CO2_uatm_amplitude = max(CO2_cal_uatm, na.rm = TRUE) - min(CO2_cal_uatm, na.rm = TRUE),
         CO2_CV = 100*(sd(CO2_cal_uatm, na.rm = TRUE) / mean(CO2_cal_uatm, na.rm = TRUE)),
         wl_mean = mean(waterLevel, na.rm = TRUE),
         wl_CV = 100*(sd(waterLevel, na.rm = TRUE) / mean(waterLevel, na.rm = TRUE)),
         precip_mean = mean(precip, na.rm = TRUE)) %>%    # not sure what this metric should be
  ungroup()

# filtering by site and converting to tstible for arima
ND_daily <- sensors_daily %>%
  filter(Site_ID == "ND") %>%
  as_tsibble(index = date)

DK_daily <- sensors_daily %>%
  filter(Site_ID == "DK" & !is.na(date)) %>%
  as_tsibble(index = date)

TS_daily <- sensors_daily %>%
  filter(Site_ID == "TS" & !is.na(date)) %>%
  as_tsibble(index = date)

# ARIMA models for ND_daily

adf.test(ND_daily$CO2_uatm_mean)
acf(ND_daily$CO2_uatm_mean)
pacf(ND_daily$CO2_uatm_mean)

# Compute difference until adf.test is significant

y_diff = diff(ND_daily$CO2_uatm_mean, differences = 2)

plot(y_diff)
pacf(y_diff)
acf(y_diff)
adf.test(y_diff)  # significant so we're able to prove stationarity

# Thus, d = 1
# PACF (p) = 3
# ACF (q) = 2

# Structure: ARIMA(p,d,q)
# ARIMA(2,1,1)

# One way to do it

# arima_1=arima(ND_daily$CO2_uatm_mean, order=c(1,0,2))
# arima_2=arima(ND_daily$CO2_uatm_mean, order=c(0,0,2))
# arima_3=arima(ND_daily$CO2_uatm_mean, order=c(1,0,0))
# 
# print(arima_1);print(arima_2);print(arima_3)

# Another way to do it from the forecast package
# Letting the model choose the orders p,d, aND_daily q

arima_m = auto.arima(ND_daily$CO2_uatm_mean)
# arima_m = auto.arima(ND_daily$CO2_uatm_mean, , max.p = 0, max.q = 1, max.d = 1, seasonal = F)
checkresiduals(arima_m)
summary(arima_m)

# Forcing the orders
# Structure: ARIMA(p,d,q)

arima_test2 <- Arima(ND_daily$CO2_uatm_mean, order = c(3,1,2))
checkresiduals(arima_test2)
summary(arima_test2)


# Adding the parameters

arimax_test <- Arima(
  ND_daily$CO2_uatm_mean,
  order = c(0, 1, 1),
  xreg = ND_daily$precip_mm
)

# Forecasting lol
forecast(arima_test2, h = 10) %>% autoplot()
