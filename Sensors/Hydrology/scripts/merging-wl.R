library(dplyr)
library(readr)

# DK1 <- read_csv("Hydrology/data/raw data/DK_SW_230408.csv")
# DK2 <- read_csv("Hydrology/data/raw data/DK_SW_240625.csv")
#
# TS1 <- read_csv("Hydrology/data/raw data/TS_SW_230408.csv")
# TS2 <- read_csv("Hydrology/data/raw data/TS_SW_240625.csv")
#
# ND1 <- read_csv("Hydrology/data/raw data/ND_SW_230408.csv")
# ND2 <- read_csv("Hydrology/data/raw data/ND_SW_240625.csv")
#
# # Merge datasets with original water level data
# merged_1 <- bind_rows(DK1, TS1, ND1)
# merged_2 <- bind_rows(DK2, TS2, ND2)
#
# merged_1$Timestamp_Corrected <- mdy_hm(merged_1$Timestamp)
# merged_2$Timestamp_Corrected <- mdy_hm(merged_2$Timestamp_Site)
#
# merged_df <- full_join(merged_1, merged_2,
#                    by = c("Timestamp_Corrected" = "Timestamp_Corrected",
#                           "Site-ID" = "Site_Name",
#                           "Stage" = "waterLevel"))

# Filter out NAs
wl_clean <- merged_df %>%
  filter(!is.na(Stage))

write_csv(wl_clean, "Hydrology/data/JL_merged WL.csv")