# Script for plotting O2-CO2 departures


# load libraries
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(paletteer)

# site color palette
# Need to fix to make color-friendly!!!

color_palette <- c("Small" = "#F8766D", 
                   "Medium" = "#00BA38", 
                   "Large" = "#619CFF")
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

# Reordering seasons
sensors$season <- factor(sensors$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Reordering site IDs
sensors$Site_ID <- factor(sensors$Site_ID, levels = c("TS", "DK", "ND"))

# Add site size descriptor column

sensors <- sensors %>%
  mutate(Site_hydroperiod = recode(Site_ID, 
                                   TS = "Small", 
                                   DK = "Medium", 
                                   ND = "Large"))

# filtering data to plot

sensors3 <- sensors %>%
  filter(waterLevel > 0.2)

plotly::ggplotly(ggplot(sensors3, aes(x=Timestamp_corrected, y = CO2_cal_uatm, color = Site_ID)) +
                   geom_point())

# filter df for the most continuous series
sensors3 <- filter(sensors3, Timestamp_corrected < "2022-08-08")

# read in data



# plotsss

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = Site_ID)) +
  geom_point(alpha = 0.2) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 100, linewidth = 1) +
  geom_hline(yintercept = 100, linewidth = 1) +
  theme(legend.position = "right", 
        strip.text = element_text(size = 20))


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
        strip.text = element_text(size = 20))

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
  ylim(0,150) +
  geom_smooth(method = "lm", color = "black")

ggsave("CO2/graphs/CO2 vs DO saturation with lm_wide_colors.jpg", width = 18, height = 8, units = "in")

ggplot(sensors3, aes(x= saturation_level, y = DO_perc)) +
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
  ylim(0,150) +
  geom_smooth(method = "lm", color = "black")




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



# plotting departure metrics

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

ggplot(sensors3, aes(x=hour, y = offset, color = Site_ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(1, 40)

# Are there seasonal patterns in the offset?
ggplot(sensors3, aes(x=season, y = offset, color = season)) +
  geom_boxplot() +
  facet_wrap(~Site_ID)

ggplot(sensors3, aes(x=Site_ID, y= offset, color = season)) +
  geom_boxplot()

ggplot(sensors3, aes(x=i.Logger_TempC, y = offset, color = season)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site_ID)

ggplot(sensors3, aes(x=Site_ID, y = offset)) +
  geom_boxplot()

ggplot(sensors3, aes(x=Site_ID, y = offset, color = season)) +
  geom_boxplot()

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = offset)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Offset") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20)) +
  scale_color_viridis_c(option = "A")

sensors3$time <- hms::as_hms(sensors3$Timestamp_corrected)

ggplot(sensors3, aes(x= saturation_level, y = DO_perc, color = date)) +
  geom_point(size = 0.6, alpha = 0.5) +
  facet_wrap(~Site_hydroperiod, ncol = 3) +
  theme +
  labs(x = CO2_sat_lab, y = "DO saturation (%)", color = "Date") +
  geom_abline(slope = -1, intercept = 200, linetype = "dashed") +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 20))
