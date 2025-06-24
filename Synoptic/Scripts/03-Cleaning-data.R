# Cleaning up data tables
# Select columns of interest, group by site and get averages of 3 reps

# For yyyymm_GHG_NEON.csv

gases <- read.csv("2021-09/202109_GHG_NEON.csv") # CHANGE FOR SAMPLING MONTH

gases_clean <- gases %>%
  group_by(Site_ID) %>%
  summarise(Site = first(Site), Sample_Type = first(Sample_Type), Sample_Date = first(Sample_Date), 
            CO2_uM = mean(dCO2.umol, na.rm = TRUE), CH4_uM = mean(dCH4.umol, na.rm = TRUE))

# Save clean, averaged spreadsheet
write.csv(gases_clean, "202109_GHG_NEON_clean.csv", row.names = FALSE)  # **CHANGE FOR SAMPLING MONTH**
