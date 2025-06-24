## Calculate equilibrium concentrations

# Look at averages of temperature 

sat_summary_SW <- SW %>%
  group_by(Site) %>%
  summarize(
    avg_temp = mean(Temp_C),
    min_temp = min(Temp_C),
    max_temp = max(Temp_C)
  )


# Create columns for atmospheric concentrations

CH4_atm <- 1.8 #ppm

CO2_atm <- 500 #ppm

# C_max_CH4 <- Pressure / kH

# I just want to get the concentration range at which gases equilibrate

## CH4 Bunsen solubility coefficient using stream temp (in C)

BT = Kt / RT

SW$CH4_BscL <- (2.7182818^(-67.196+(99.162*(100/(SW$Temp_C+273)))+(27.902*log((SW$Temp_C+273)/100))))

## Headspace Correction
## Assuming a water volume of 20
SW$CH4_HC <- (SW$CH4_atm/(0.0821*273.15))*(20/1000)

## Liquid Correction assuming a volume of 30
SW$CH4_LC <- (((SW$CH4_atm*SW$CH4_BscL*(SW$Atm_pressure_mBar)/10)/(0.0821*273.15))*(20/1000))
SW$CH4_LC <- (((SW$CH4_atm*SW$CH4_BscL*(101)*1)/(0.0821*273.15))*(20/1000))

## CH4_eq in uM
SW$CH4_eq <- (SW$CH4_HC+SW$CH4_LC)/(20/1000)

## CO2 Bunsen solubility coefficient using stream temp (in C)
SW$CO2_BscL <- (2.7182818^(-58.0931+(90.5069*(100/(SW$Temp_C+273)))+(22.294*log((SW$Temp_C+273)/100))))*((0.0821*273.15)+((-1636.75+(12.0408*273.15)-(3.27957*0.01*273.15*273.15)+(3.16528*0.00001*273.15*273.15*273.15))/1000))

SW$CO2_BscL2 <- -58.0931 + 90.5069/(100/(SW$Temp_C+273.15)) + (22.2940 * log(((SW$Temp_C+273.15)/100))) + (0.02776 - 0.02588 * ((SW$Temp_C +273.15)/100) + 0.0050578 * ((SW$Temp_C+273.15)/100)^2)

S[0.027766 -0.025888 ((T+273.15)/100) + 0.0050578 ((T+273.15)/100)^2]
(0.02776 - 0.02588 * ((SW$Temp_C +273.15)/100) + 0.0050578 * ((SW$Temp_C+273.15)/100)^2)
## Headspace Correction
## Assuming a water volume of 30
SW$CO2_HC <- (SW$CO2_atm/(0.0821*273.15))*(20/1000)
## Liquid Correction assuming a volume of 20
SW$CO2_LC <- (((SW$CO2_atm*SW$CO2_BscL2*(101)/10)/(0.0821*273.15))*(20/1000))
## CO2_eq in uM
SW$CO2_eq <- (SW$CO2_HC+SW$CO2_LC)/(20/1000)

## Calculate percent saturation

# saturation = observed concentration / equilibrium concentration

SW$CH4_sat_per <- (SW$CH4_uM / SW$CH4_eq)*100

SW$CO2_sat_per <- (SW$CO2_uM / SW$CO2_eq)*100

mean(SW$CO2_sat_per, na.rm = TRUE)
mean(SW$CH4_sat_per, na.rm = TRUE)

ggplot(SW, aes(x= Site, y = CO2_sat_per)) +
  geom_boxplot(fill = "gray", position = position_dodge(1)) +
  labs(x = "", y = "CO2 saturation (%)") +
  geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0) +
  theme_bw() + theme(axis.ticks.x=element_blank()) + theme(axis.text.x=element_blank()) +
  theme

ggsave("Graphs/Time-series/CO2_sat.jpg")

ggplot(SW, aes(x= Site, y = CH4_sat_per)) +
  geom_boxplot(fill = "gray", position = position_dodge(1)) +
  labs(x = "Sampling date", y = "CH4 saturation (%)") +
  geom_hline(yintercept = 100, size = 2, alpha = 0.6) +
  geom_jitter(size = 3, width = 0.1) +
  theme_bw() +
  scale_y_log10() +
  theme

write.csv(SW, "GHG_Percent_saturation.csv")

ggplot(SW, aes(x=Temp_C, y= CH4_sat_per)) +
  geom_point()

# Look at moments where it's undersaturated

SW_undersaturated <- filter(SW, CH4_sat_per < 150)
