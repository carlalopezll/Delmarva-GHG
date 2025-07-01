# Script that uses ARIMA models to look at the influence of precipitation on CO2 in Delmarva wetlands
# Carla López Lloreda

# This would be better as a Markdown

# load libraries
library(tseries)
library(forecast) # for auto.arima()
library(fable) # for ARIMA()
library(tsibble)
library(readr)
library(broom)

# read in high-frequency data
sensors_hf <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

sensors_hf <- mutate(sensors_hf, date = as.Date(Timestamp_corrected))

# read in daily data w/ metrics
sensors_daily <- read_csv("CO2/data/processed data/sensors_daily.csv")

#### Daily means ####
# Suggestion by Cayelan was to use daily means, amplitude range, instead of the high-frequency data

# using just a subset of data without gaps
sensors_daily_subset <- sensors_daily %>%
  filter(date > "2021-09-28" & date < "2021-11-14")

sensors_hf_subset <- sensors_hf %>%
  filter(date > "2021-09-28" & date < "2021-11-14")

# filtering by site and converting to tstible for arima
ND_daily <- sensors_daily_subset %>%
  filter(Site_ID == "ND") %>%
  as_tsibble(index = date)
DK_daily <- sensors_daily_subset %>%
  filter(Site_ID == "DK" & !is.na(date)) %>%
  as_tsibble(index = date)
TS_daily <- sensors_daily_subset %>%
  filter(Site_ID == "TS" & !is.na(date)) %>%
  as_tsibble(index = date)


ND_hf <- sensors_hf_subset %>%
  filter(Site_ID == "ND") %>%
  as_tsibble(index = date)

#### ARIMA models for ND daily ####

adf.test(ND_daily$CO2_uatm_mean)
acf(ND_daily$CO2_uatm_mean)
pacf(ND_daily$CO2_uatm_mean)

# Compute difference until adf.test is significant

y_diff = diff(ND_daily$CO2_uatm_mean, differences = 2)

plot(y_diff)
pacf(y_diff) # determines p
acf(y_diff) # determines q
adf.test(y_diff)  # determines d, if significant we're able to confirm stationarity

# Thus, d = 2
# PACF (p) = 3
# ACF (q) = 2
# Structure: ARIMA(p,d,q)

# One way to do it

# arima_1=arima(ND_daily$CO2_uatm_mean, order=c(1,0,2))
# arima_2=arima(ND_daily$CO2_uatm_mean, order=c(0,0,2))
# arima_3=arima(ND_daily$CO2_uatm_mean, order=c(1,0,0))
# 
# print(arima_1);print(arima_2);print(arima_3)

# Another way to do it from the forecast package
# Letting the model choose the orders p,d, q

arima_m = auto.arima(ND_daily$CO2_uatm_mean)
checkresiduals(arima_m)
summary(arima_m)

# if you wanted to fix max values of p, d, q, use this
# arima_m = auto.arima(ND_daily$CO2_uatm_mean, , max.p = 0, max.q = 1, max.d = 1, seasonal = F)

# Forcing the orders using Arima() from the forecast package
# Structure: ARIMA(p,d,q)

arima_test <- Arima(ND_daily$CO2_uatm_mean, order = c(3,1,2))
checkresiduals(arima_test)
summary(arima_test)


# looking at influence of precip on water level


# Adding the parameters to the model

str(ND_daily)

# Define the matrix of external regressors
xreg_vars <- as.matrix(ND_daily[, c("wl_mean", "wl_CV", "delta_wl")])

# Fit ARIMAX model
arimax_model <- Arima(ND_daily$CO2_uatm_mean, order = c(3,1,2), xreg = xreg_vars)

# Check residuals
checkresiduals(arimax_model)

# Summary of model
summary(arimax_model)

autoplot(residuals(arimax_model))
forecast::autoplot(forecast(arimax_model, xreg = xreg_vars))


# Create data frame with observed and fitted values
plot_df <- data.frame(
  DATE = ND_daily$date,
  Observed = ND_daily$CO2_uatm_mean,
  Fitted = fitted(arima_test)
)

# Plot
ggplot(plot_df, aes(x = DATE)) +
  geom_line(aes(y = Observed), color = "black") +
  geom_line(aes(y = Fitted), color = "blue") +
  labs(title = "Observed vs Fitted CO₂", y = "CO₂ (µatm)", x = "Date") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")


# Forecasting lol
forecast(arima_test, h = 10) %>% autoplot()





#### ARIMA models for ND high-frequency ####

#### Daily means ####
# Suggestion by Cayelan was to use daily means, amplitude range, instead of the high-frequency data

# using just a subset of data without gaps
sensors_hf_subset <- sensors_hf %>%
  filter(date > "2021-09-28" & date < "2021-11-14")

# filtering by site and converting to tstible for arima
ND_daily <- sensors_hf_subset %>%
  filter(Site_ID == "ND") %>%
  as_tsibble(index = date)

DK_daily <- sensors_hf_subset %>%
  filter(Site_ID == "DK" & !is.na(date)) %>%
  as_tsibble(index = date)

TS_daily <- sensors_hf_subset %>%
  filter(Site_ID == "TS" & !is.na(date)) %>%
  as_tsibble(index = date)

#### ARIMA models for ND daily ####

adf.test(ND_daily$CO2_uatm_mean)
acf(ND_daily$CO2_uatm_mean)
pacf(ND_daily$CO2_uatm_mean)

# Compute difference until adf.test is significant

y_diff = diff(ND_daily$CO2_uatm_mean, differences = 2)

plot(y_diff)
pacf(y_diff) # determines p
acf(y_diff) # determines q
adf.test(y_diff)  # determines d, if significant we're able to confirm stationarity

# Thus, d = 2
# PACF (p) = 3
# ACF (q) = 2
# Structure: ARIMA(p,d,q)

# One way to do it

# arima_1=arima(ND_daily$CO2_uatm_mean, order=c(1,0,2))
# arima_2=arima(ND_daily$CO2_uatm_mean, order=c(0,0,2))
# arima_3=arima(ND_daily$CO2_uatm_mean, order=c(1,0,0))
# 
# print(arima_1);print(arima_2);print(arima_3)

# Another way to do it from the forecast package
# Letting the model choose the orders p,d, q

arima_m = auto.arima(ND_daily$CO2_uatm_mean)
checkresiduals(arima_m)
summary(arima_m)

# if you wanted to fix max values of p, d, q, use this
# arima_m = auto.arima(ND_daily$CO2_uatm_mean, , max.p = 0, max.q = 1, max.d = 1, seasonal = F)

# Forcing the orders using Arima() from the forecast package
# Structure: ARIMA(p,d,q)

arima_test <- Arima(ND_daily$CO2_uatm_mean, order = c(3,1,2))
checkresiduals(arima_test)
summary(arima_test)

# Adding the parameters to the model

str(ND_daily)

# Define the matrix of external regressors
xreg_vars <- as.matrix(ND_daily[, c("wl_mean", "wl_CV", "delta_wl")])

# Fit ARIMAX model
arimax_model <- Arima(ND_daily$CO2_uatm_mean, order = c(3,1,2), xreg = xreg_vars)

# Check residuals
checkresiduals(arimax_model)

# Summary of model
summary(arimax_model)

autoplot(residuals(arimax_model))
forecast::autoplot(forecast(arimax_model, xreg = xreg_vars))


# Forecasting lol
forecast(arima_test, h = 10) %>% autoplot()
