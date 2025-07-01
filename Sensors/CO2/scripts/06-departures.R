library(ggplot2)
library(readr)
library(dplyr)

# Read in merged script

sensors <- read_csv("CO2/data/processed data/merged sensors_250412.csv")

# Reordering seasons
sensors$season <- factor(sensors$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Reordering site IDs
sensors$Site_ID <- factor(sensors$Site_ID, levels = c("TS", "DK", "ND"))

# Filter so that we use only the time with precip and CO2 data
# sensors <- filter(sensors, timestamp < "2022-08-04")

# Rename sites

sensors <- sensors %>%
  mutate(Site_hydroperiod = recode(Site_ID, 
                                   TS = "Small", 
                                   DK = "Medium", 
                                   ND = "Large"))

# Constants
MOLAR_MASS_CO2 <- 44.01 # g/mol (molecular weight of CO2)
SATURATION_CONCENTRATION <- 1.3 # mmol/L, equilibrium solubility of CO2 at 25°C
SATURATION_CONCENTRATION_MOLAR <- SATURATION_CONCENTRATION / 1000 # Convert to mol/L (1 mmol/L = 0.001 mol/L)

# Convert ppm to molarity (mol/L)
convert_ppm_to_molarity <- function(ppm) {
  molarity <- ppm / (MOLAR_MASS_CO2 * 1000) # ppm is mg/L, molecular weight is in g/mol, and we need mol/L
  return(molarity)
}

# Calculate CO2 saturation for each site and timestamp in sensors2
sensors2 <- sensors %>%
  mutate(
    # Convert CO2_ppm_corr to molarity (mol/L)
    co2_molarity = convert_ppm_to_molarity(CO2_ppm_corr),
    
    # Calculate saturation level: measured concentration / saturation concentration
    saturation_level = co2_molarity / SATURATION_CONCENTRATION_MOLAR
  )

sensors2$DO_Concentration_uM <- sensors2$DO_conc_mgL * 1000 / 31.999
sensors2$DO_sat <- sensors2$DO_perc * 1000 / 31.999

# Need to convert CO2 from ppm to uM
sensors2$CO2_Conc_uM <- sensors2$CO2_ppm_corr * 1000 / 44

# Calculate CO2 saturation
# Henry's law constants and temperature dependence from Sander (2015)

ckHCO2 = 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
cdHdTCO2 = 2400 #K, range: 2300 - 2600
cT0 = 298.15 #Henry's law constant T0

# allData$CO2.sat = (ckHCO2 * exp(cdHdTCO2*(1/(allData$NEON.temp + 273.15) - 1/cT0))) * 
#   allData$co2.atm.ppmv * allData$barPres

sensors2$CO2_sat <- (ckHCO2 * exp(cdHdTCO2*(1/(sensors2$Logger_TempC + 273.15) - 1/cT0))) * 420 * sensors2$Site_AbsPressure.kPa

mean(sensors2$Site_AbsPressure.kPa, na.rm = T)*10
# 1109.355

sensors2$CO2_sat <- (ckHCO2 * exp(cdHdTCO2*(1/(15 + 273.15) - 1/cT0))) * 420 * 1109.35

# #calculate departures:
# allData$O2.departure<-allData$dissolvedOxygen - allData$DO.sat
# allData$CO2.departure<-allData$CO2.uM.corr - allData$CO2.sat
# #change to if-else if not all sites are using corr data
# allData$CO2.departure.raw<-allData$CO2.uM - allData$CO2.sat
# allData$site=as.factor(allData$site)


# I don't think I'm doing the departure calculations right
sensors2$O2_departure <- sensors2$DO_Concentration_uM / sensors2$DO_sat
sensors2$CO2_departure <- sensors2$CO2_Conc_uM / sensors2$CO2_sat

# Plotting DO-CO2

# Theme
theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

CO2_lab <- expression(paste("C","O"[2]^{}*" (ppm)"))
CO2_sat_lab <- expression(paste("C","O"[2]^{}*" saturation (%)"))

sensors3 <- sensors2 %>%
  filter(waterLevel > 0.2)

# Load library
library(paletteer)

# Need to fix to make color-friendly!!!

color_palette <- c("Small" = "#F8766D", 
                   "Medium" = "#00BA38", 
                   "Large" = "#619CFF")


# Blank figure

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = Site_hydroperiod)) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "none", 
        strip.text = element_text(size = 20)) +
  ylim(0,125) +
  xlim(20, 500)

ggsave("CO2/graphs/CO2 vs DO saturation_wide_blank.jpg", width = 18, height = 8, units = "in")


# Step 1: Fit linear models and extract stats by Site_hydroperiod
lm_annotations <- sensors3 %>%
  group_by(Site_hydroperiod) %>%
  do({
    model <- lm(DO_perc ~ CO2_sat, data = .)
    stats <- tidy(model)
    model_summary <- summary(model)
    slope <- stats$estimate[stats$term == "saturation_level"]
    intercept <- stats$estimate[stats$term == "(Intercept)"]
    p_val <- stats$p.value[stats$term == "saturation_level"]
    r2 <- model_summary$r.squared
    
    # Return annotation label and placeholder coordinates
    tibble(Site_hydroperiod = unique(.$Site_hydroperiod),
           label = paste0("y = ", round(slope, 2), "x + ", round(intercept, 2),
                          "\nR² = ", round(r2, 2),
                          "\np = ", signif(p_val, 2)),
           x = 300,  # Adjust these based on your data range
           y = 100)
  })





# DO-CO2 for each individual site

ggplot(sensors3, aes(x= saturation_level, y = DO_perc)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "none", 
        strip.text = element_text(size = 20)) +
  ylim(0,125) +
  xlim(20, 500) +
  geom_smooth(method = "lm", color = "black") +
  geom_text(data = lm_annotations, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4)

ggsave("CO2/graphs/CO2 vs DO saturation with lm_wide_w stats.jpg", width = 18, height = 8, units = "in")

# DO-CO2 for each individual site

ggplot(sensors3, aes(x= saturation_level, y = DO_perc)) +
  geom_point(size = 0.6, alpha = 0.5, color = "black") +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "none", 
        strip.text = element_text(size = 20)) +
  ylim(0,125) +
  xlim(20, 500)

ggsave("CO2/graphs/CO2 vs DO saturation with lm_wide_black.jpg", width = 18, height = 8, units = "in")

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = Site_hydroperiod)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "none", 
        strip.text = element_text(size = 20)) +
  ylim(0,125) +
  xlim(20, 500) +
  geom_smooth(method = "lm", color = "black")

ggsave("CO2/graphs/CO2 vs DO saturation with lm_wide_colors.jpg", width = 18, height = 8, units = "in")


# DO-CO2 for each individual site colored by season
ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = GP_TempC)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Water level (m)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20)) +
  scale_color_viridis_c(option = "C") +
  ylim(0,125) +
  xlim(20, 500)

ggsave("CO2/graphs/CO2 vs DO saturation by season_wide.svg", width = 18, height = 8, units = "in")


# DO-CO2 for each individual site colored by water level
ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = waterLevel)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Water level (m)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  scale_color_viridis_c(option = "C") +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20)) +
  ylim(0,125) +
  xlim(20, 500) +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.5))

ggsave("CO2/graphs/CO2 vs DO saturation by water level_wide.jpg", width = 18, height = 8, units = "in")

# During which season does low and high water level occur for each site?
ggplot(sensors, aes(x= Site_ID, y = waterLevel, fill = season)) +
  geom_boxplot()



lm_DO_CO2_slopes <- sensors3 %>%
  group_by(Site_ID) %>%
  do({
    model <- lm(DO_perc ~ saturation_level, data = .)
    model_summary <- summary(model)
    tidy_model <- tidy(model)
    tidy_model$r_squared <- model_summary$r.squared
    tidy_model
  }) %>%
  filter(term == "saturation_level") %>%  # Keep only slope terms
  select(Site_ID, slope = estimate, p_value = p.value, r_squared)



# DO-CO2 for the 3 sites together
ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = Site_ID)) +
  geom_point(alpha = 0.5) +
  theme +
  labs(x ="CO2 saturation (%)", y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  xlim(0,800)

ggsave("CO2/graphs/CO2 vs DO saturation_all.jpg", width = 10, height = 6, units = "in")





ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = waterLevel)) +
  geom_point(alpha = 0.5) +
  stat_cor(aes(group = Site_ID, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 400, label.y= 160, size = 5, 
           color = "black") +
  facet_wrap(~Site_ID) +
  theme +
  labs(x ="CO2 saturation (%)", y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  xlim(0,800)

ggsave("CO2/graphs/CO2 vs DO saturation.jpg", width = 17, height = 6, units = "in")

sensors3$DO_CO2 <- sensors3$DO_Concentration_uM/sensors3$CO2_Conc_uM

sensors3$euclidean_distance <- sqrt(sensors3$DO_perc^2 + sensors3$saturation_level^2)

str(sensors3$euclidean_distance)



# calculating departure metrics

sensors3 <- sensors3 %>%
  mutate(offset = abs(sensors3$saturation_level + sensors3$DO_perc) / sqrt(2))

ggplot(sensors3, aes(x=Timestamp_corrected, y = offset, color = Site_ID)) +
  geom_point() +
  geom_smooth()

ggplot(sensors3, aes(x=waterLevel, y = offset, color = Site_ID)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sensors3, aes(x=precip_mm, y = offset, color = Site_ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(1, 40)

ggplot(sensors3, aes(x=Site_ID, y= offset, color = season)) +
  geom_boxplot()

ggplot(sensors3, aes(x=i.Logger_TempC, y = offset, color = season)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site_ID)

ggplot(sensors3, aes(x=Site_ID, y = offset, color = season)) +
  geom_boxplot()

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = departure_1m1)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Water level (m)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20)) +
  scale_color_viridis_c(option = "C")


ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = departure_1m1)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Water level (m)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20))
