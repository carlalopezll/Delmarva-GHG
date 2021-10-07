# Coefficients of variation


# Omg this is going to need a for loop.....

cv_CO2 <- sd(All_2011$Cl_mg_L) / mean(All_2011$Cl_mg_L) * 100

cv_CH4 <- sd(All_2011$CH4) / mean(All_2011$CH4) * 100   # why doesn't it work for CO2 and CH4?
