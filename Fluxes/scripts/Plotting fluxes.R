

fluxes <- read.csv("Soil cores_Fluxes.csv")

ggplot(fluxes, aes(x= Timepoint, y= co2_flux_umolCm2s)) +
  geom_boxplot()

ggplot(fluxes, aes(x= Core, y= co2_flux_umolCm2s)) +
  geom_boxplot()

fluxes %>%
  filter(ch4_R2 > 0.7) %>%
  ggplot(aes(x = Date_real, y=ch4_flux_umolCm2s, color = Core))+
  geom_smooth()+
  geom_point()+
  ylab("CH4 flux (µumol/m2/s)")

fluxes %>%
  filter(co2_R2 > 0.7) %>%
  ggplot(aes(x = Date_real, y=co2_flux_umolCm2s, color = Core))+
  geom_smooth()+
  geom_point()+
  ylab("CO2 flux (µumol/m2/s)")

fluxes %>%
  filter(co2_R2 > 0.7) %>%
  ggplot(aes(x = Date_real, y=co2_flux_umolCm2s, color = Core))+
  geom_boxplot() +
  ylab("CO2 flux (µumol/m2/s)")

