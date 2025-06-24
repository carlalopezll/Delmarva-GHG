library(tseries)
library(forecast) # for auto.arima()
library(fable) # for ARIMA()
library(tsibble)

sensors_15min <- sensors %>%
  mutate(date = as.Date(Timestamp))

#### Daily means ####

# Looking at a subset of data without gaps
sensors_subset <- sensors %>%
  filter(date > "2021-09-28" & date < "2021-11-14")

# Working only with the daily mean data
sensors_daily <- sensors_subset %>%
  mutate(date = as.Date(Timestamp)) %>%  # Extract date from timestamp
  group_by(Site_ID, date) %>%               # Group by Site and Date
  summarise(CO2_uatm_mean = mean(CO2_cal_uatm, na.rm = TRUE)) %>%
  ungroup()

ND_daily <- sensors_daily %>%
  filter(Site_ID == "ND") %>%
  as_tsibble(index = date)

DK_daily <- sensors_daily %>%
  filter(Site_ID == "DK" & !is.na(date)) %>%
  as_tsibble(index = date)

TS_daily <- sensors_daily %>%
  filter(Site_ID == "TS" & !is.na(date)) %>%
  as_tsibble(index = date)

a <- ggplot(ND_daily, aes(x = date, y = CO2_uatm_mean)) +
  geom_point()

b <- ggplot(DK_daily, aes(x = date, y = CO2_uatm_mean)) +
  geom_point()

c <- ggplot(TS_daily, aes(x = date, y = CO2_uatm_mean)) +
  geom_point()

cowplot::plot_grid(a,b,c, nrow=3)

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
