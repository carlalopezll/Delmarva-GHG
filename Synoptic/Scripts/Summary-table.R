library(tidyverse)
library(dplyr)


# Making a table of averages, ranges, and standard deviations
# How to get a mean per site for all the variables? + include all data info

site_avg_SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  group_by(Site_ID) %>%
  summarise(Sample_Type = first(Sample_Type), 
            Site = first(Site),
            avg_CO2 = mean(CO2_uM),
            std_CO2 = sd(CO2_uM),
            min_CO2 = min(CO2_uM),
            max_CO2 = max(CO2_uM),
            avg_CH4 = mean(CH4_uM),
            std_CH4 = sd(CH4_uM),
            min_CH4 = min(CH4_uM),
            max_CH4 = max(CH4_uM))

library(confintr)

se_mean(SW$CO2_uM)
se_mean(SW$CH4_uM)

avg_SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  summarise(Sample_Type = first(Sample_Type),
            avg_CO2 = mean(CO2_uM),
            std_CO2 = sd(CO2_uM),
            min_CO2 = min(CO2_uM),
            max_CO2 = max(CO2_uM),
            avg_CH4 = mean(CH4_uM),
            std_CH4 = sd(CH4_uM),
            min_CH4 = min(CH4_uM),
            max_CH4 = max(CH4_uM))


write.csv(summary_SW, "GHG summary.csv", row.names = F)