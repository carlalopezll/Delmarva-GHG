# Cleaning up data tables
# Select columns of interest, group by site and get averages of 3 reps


# For yyyymm_GHG_Wetlands.csv

samp <- read.csv("2021-06/202106_GHG_Wetlands.csv") # CHANGE FOR SAMPLING MONTH

samp_clean <- samp %>%
  group_by(Site_ID) %>%
  summarise(Site = first(Site), Sample_Type = first(Sample_Type), Sample_Date = first(Sample_Date), 
            CO2_uM = mean(wCO2_uM_med, na.rm = TRUE), CH4_uM = mean(wCH4_uM_med, na.rm = TRUE))

# Save clean, averaged spreadsheet
write.csv(samp_clean, "202109_GHG_Clean.csv", row.names = FALSE)  # **CHANGE FOR SAMPLING MONTH**


# For yyyymm_GHG_NEON.csv

gases <- read.csv("2021-09/202109_GHG_NEON.csv") # CHANGE FOR SAMPLING MONTH

gases_clean <- gases %>%
  group_by(Site_ID) %>%
  summarise(Site = first(Site), Sample_Type = first(Sample_Type), Sample_Date = first(Sample_Date), 
            CO2_uM = mean(dCO2.umol, na.rm = TRUE), CH4_uM = mean(dCH4.umol, na.rm = TRUE))

# Save clean, averaged spreadsheet
write.csv(gases_clean, "202109_GHG_NEON_clean.csv", row.names = FALSE)  # **CHANGE FOR SAMPLING MONTH**

# Fixing dates
# GHG_all$Sample_Date <- as.POSIXct(strptime(as.character(GHG_all$Sample_Date,"%d/%m/%Y"), format = "%Y%m%d"))
