fluxes <- read.csv("DMV fluxes.csv")

# Reordering site IDs
fluxes$Site <- factor(fluxes$Site, levels = c("TS", "DK", "ND"))

library(ggplot2)

ggplot(fluxes, aes(x= Site, y = flux_CO2_mmol_m2_d)) +
  geom_boxplot() +
  geom_jitter()

ggsave("CO2 fluxes.jpg")

ggplot(fluxes, aes(x= Site, y = flux_CH4_mmol_m2_d)) +
  geom_boxplot() +
  geom_jitter() +
  scale_y_log10()

ggsave("CH4 fluxes.jpg")

