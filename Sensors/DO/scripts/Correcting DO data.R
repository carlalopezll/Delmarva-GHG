# Correcting O2 data for Delmarva wetlands


# Set your working directory
# Needs to change for each user!

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Sensors/Data/Jackson Lane/DO_Temp")

# Read in data

do <- read.csv("DK_SW_PME_20210417_20210701.csv")

# Calculating DO sat, as in Garcia and Gordon 1992 L&O, using temp and barometric pressure (mmHg)

# This function isn't working, need to check formula
osat <- function(temp, bp){ 
  ts <- log((298.15-temp) / (273.15 + temp))
  a0 <- 2.00907
  a1 <- 3.22014
  a2 <- 4.0501
  a3 <- 4.94457
  a4 <- -0.256847
  a5 <- 3.88767
  u <- 10^(8.10765-(1750.286/(235+temp)))* (exp(a0 + a1*ts + a2*ts^2 + a3*ts^3 + a4*ts^4+a5*ts^5))*((bp-u)/(760-u))*1.42905
} 

# Calculate % saturation of DO using the osat function + your temp and bp data

# Is bp pressure at time of "calibration" or average of deployment location?

do$bp = 751

do$DO_sat <- osat(temp = do$Temp_C, bp = do$bp) #bpst is in mmHg
