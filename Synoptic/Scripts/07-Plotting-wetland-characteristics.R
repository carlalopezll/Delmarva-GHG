# Script for looking at coefficients of variation and GHG concentrations
# against wetland characteristics

# Carla Lopez Lloreda
# Updated 3/27/2022

library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(MetBrewer) # cool color palette
library(olsrr) # for residual tests
library(ggpubr) # for p values and r-squared on graph
library(gridExtra)
library(cowplot)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

wetland_info <- read_csv("Master spreadsheet.csv")

#### CVs with wetland metrics ####

CV_site <- read_csv("Site CV_240801.csv") # sheet with calculated CVs


# Join CVs and wetland info by site ID
join <- inner_join(CV_site, wetland_info, by = c("Site_ID" = "Site_ID", "Sample_Type" = "Sample_Type", "Site" = "Site"))

# Save as csv
# These csv has wetland metrics + CO2 and CH4 stats

write_csv(join, "Wetland characteristics.csv")

######################################################

# Load this if not adding new files or changing anything
# join <- read.csv("Wetland characteristics.csv")


# Subset by sample type
SW <- filter(join, Sample_Type == "SW")
UW <- filter(join, Sample_Type == "UW")
  

### Start plotting 

# Theme stuff
theme <- theme_bw() +
  theme(text = element_text(size = 20))
# theme(legend.position = "none", panel.border = element_blank(), 
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                    axis.line = element_line(colour = "black"))


# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

CO2_CV_lab <- expression(paste("C","O"[2]^{}*" CV (%)"))
CH4_CV_lab <- expression(paste("C","H"[4]^{}*" CV (%)"))

# Area
wetland_area_lab <- bquote("Wetland area" ~(m^2))

#### Looking at wetland characteristics ####

ggplot(SW, aes(x = area_m2, y = wetland_storage_volume_m3)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  geom_label(aes(label=Site)) +
  labs(x = "Area (m^2)", y = "Wetland storage volume (m^3)") +
  theme

ggplot(wetland_info, aes(x = area_m2, y = wetland_storage_volume_m3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Area (m^2)", y = "Wetland storage volume (m^3)") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 1500, size = 5) +
  theme

fit <- lm(formula = area_m2 ~ wetland_storage_volume_m3, data = wetland_info)

ggsave("Graphs/Regressions/Volume vs area.jpg")

ggplot(wetland_info, aes(x = area_m2, y = p_a_ratio)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Area (m^2)", y = "Perimeter:area ratio") +
  theme

ggsave("Graphs/Regressions/p_a vs area.jpg")

# Adding a polynomial regression

fit <- lm(formula = p_a_ratio ~ poly(area_m2, 2), data = wetland_info)

summary(fit)

ggplot(wetland_info, aes(area_m2, p_a_ratio)) +
  geom_point() +
  geom_smooth(aes(area_m2, predict(fit))) +
  labs(x = "Area (m^2)", y = "Perimeter:area ratio") +
  theme

ggsave("Graphs/Regressions/p_a vs area_line.jpg")

# Are all 1st order wetlands small? NO

ggplot(wetland_info, aes(x = wet_order, y = area_m2)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  labs(x = "Wetland order", y = "Wetland area (m^2)") +
  theme

ggsave("Graphs/order vs area.jpg")

# Do larger wetland have less canopy cover? YES

ggplot(wetland_info, aes(x = area_m2, y = mean_gap_frac)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  labs(x = "log-Area (m^2)", y = "Mean gap fraction") +
  scale_x_log10() +
  theme 

ggsave("Graphs/canopy cover vs area.jpg")



# Histograms of landscape variables

p1 <- ggplot(SW, aes(x = area_m2)) +
  labs(x = wetland_area_lab, y = "Count", tag = "a)") + 
  theme +
  scale_x_log10() +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(area_m2)),
             linetype="dashed")
p1

mean(SW$area_m2) # 1430.529
range(join$area_m2) # 379.0 - 6410.0

ggsave("Graphs/Area histogram.jpg")

p2 <- ggplot(SW, aes(x = wl_mean)) +
  labs(x = "Wetland water level (m)", y = "Count", tag = "b)") + 
  theme +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(wl_mean)),
             linetype="dashed")
p2

mean(CV_SW$wl_mean) # 0.47989
range(CV_SW$wl_mean) # 0.2183897 - 0.8193264

ggsave("Graphs/WL histogram.jpg")

p3 <- ggplot(SW, aes(x = area_m2, y = wl_mean)) +
  geom_point(size = 4) +
  labs(x = wetland_area_lab, y = "Water level mean (m)", tag = "c)") +
  theme +
  scale_color_brewer(palette = "Dark2", name = "Dominant vegetation") +
  scale_x_log10() +
  theme(
    legend.position = c(.99, .94),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "transparent"),
  )
p3

ggsave("Graphs/MS/WL vs area.jpg")

p <- grid.arrange(p1, p2, p3, ncol = 2, 
                  layout_matrix = cbind(c(1,1,2,2), c(3,3,3)))

ggsave("Graphs/MS/Figure 2_new.jpg", p)


# Are smaller wetlands more hydrologically variable??
# Depends on how you look at it
# Forested wetlands (excluding DF, TI, FN) and with the exception of OB, YES

a1 <- ggplot(SW, aes(x = area_m2, y = CV_wl)) +
  geom_point(size = 3) +
  labs(x = wetland_area_lab, y = "CV of water level (%)") +
  theme

a1

a2 <- ggplot(SW, aes(x = area_m2, y = CV_wl)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlim(NA, 2000) +
  labs(x = wetland_area_lab, y = "CV of water level (%)") +
  theme

a2

a <- grid.arrange(a1, a2, ncol = 1)

ggsave("Graphs/MS/WL CV vs area.jpg", a, width = 12, height = 8, dpi = 300)


# Plotting GHG CVs against water level CV
# Are more hydrologically variable wetlands more variable for GHG?

ggplot(SW, aes(x = CV_wl, y = CV_CO2, color = area_m2)) +
  geom_point() +
  scale_x_log10() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Hokusai1")) +
  labs(x = "Water level CV (%)", y = "CV of CO2 (%)") +
  theme +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           size = 5)

ggsave("Graphs/CV CO2 vs CV WL.jpg")

p2 <- ggplot(SW, aes(x = CV_wl, y = CV_CO2)) +
  geom_point(size=3) +
  scale_x_log10() +
  labs(x = "", y = CO2_CV_lab, tag = "a)") +
  theme +
  theme(legend.position = "none")

summary(lm(CV_CO2~CV_wl, SW))

p2

ggsave("Graphs/MS/CV CO2 vs CV WL.jpg")

# YES FOR CH4

ggplot(SW, aes(x = CV_wl, y = CV_CH4, color = area_m2)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  geom_label(aes(label=Site)) +
  labs(x = "CV of water level (%)", y = "CV of CH4 (%)") +
  theme +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           size = 5)

p3 <- ggplot(CV_SW, aes(x = CV_wl, y = CV_CH4)) +
  geom_point(size=3) +
  geom_smooth(method = "lm") +
  labs(x = "Water level CV (%)", y = CH4_CV_lab, tag = "b)") +
  theme +
  theme(legend.position = "none") +
  scale_x_log10()

summary(lm(CV_CH4~CV_wl, SW))

p3

ggsave("Graphs/MS/CV CH4 vs CV WL.jpg")

# Plotting CV graphs together

g <- grid.arrange(arrangeGrob(p2, p3), ncol = 1)

ggsave("Graphs/MS/WL CV vs GHG CV.jpg", g)

# Plotting wetland water level means with GHG means

ggplot(SW, aes(x = wl_mean, y = CO2_mean, color = area_m2)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Hokusai1")) +
  labs(x = "Water level mean (m)", y = CO2_lab, color = "log(Wetland area (m2))", tag = "a)") +
  theme +
  theme(legend.position = c(0.90, .95), # For legend position
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill= "transparent"))

ggsave("Graphs/MS/CO2 conc vs WL mean.jpg")


ggplot(SW, aes(x = wl_mean, y = CH4_mean, color = area_m2)) +
  geom_point() +
  geom_smooth() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Hokusai1"))+
  labs(x = "Water level mean (m)", y = CH4_lab) + 
  theme

ggsave("Graphs/MS/CH4 conc vs WL mean.jpg")

# Water level CV vs GHG concentrations

ggplot(SW, aes(x = CV_wl, y = CO2_mean)) +
  geom_point(size=3) +
  geom_smooth(method = "lm") +
  labs(x = "CV of water level (%)", y = CO2_lab) +
  scale_x_log10() +
  theme

summary(lm(CO2_mean~CV_wl, SW))

ggsave("Graphs/CO2 conc vs CV WL.jpg")

ggplot(SW, aes(x = CV_wl, y = CH4_mean)) +
  geom_point(size=3) +
  labs(x = "CV of water level (%)", y = CH4_lab) + 
  scale_x_log10() +
  theme

summary(lm(CH4_mean~CV_wl, SW))

ggsave("Graphs/CH4 conc vs CV WL.jpg")


#### Plotting wetland variables with GHG concentrations ####

# Do smaller wetlands have higher CO2 concentrations?

a1 <- ggplot(SW, aes(x = area_m2, y = CO2_mean)) +
  geom_point(size = 3) +
  labs(x = "", y = CO2_lab, tag = "a)") +
  scale_x_log10() +
  theme(legend.position = "none") +
  theme

summary(lm(CO2_mean ~ area_m2, SW))

a1

ggsave("Graphs/CO2 conc vs area.jpg")


a2 <- ggplot(SW, aes(x = area_m2, y = CH4_mean)) +
  geom_point(size= 3) +
  geom_smooth(method = "lm") +
  labs(x = bquote("Wetland area" ~(m^2)), y = CH4_lab, tag = "d)") +
  scale_x_log10() +
  theme

summary(lm(CH4_mean ~ area_m2, SW))

a2

model <- summary(lm(CH4_mean ~ area_m2, SW))
p_values <- coef(model)[, "Pr(>|t|)"]
p_values

adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
adjusted_p_values

ggsave("Graphs/CH4 conc vs area.jpg")

a <- plot_grid(a1, a2, ncol = 1, align = "v", rel_heights = c(1, 1))
a

ggsave("Graphs/MS/area with GHGs.jpg", t)



# With perimeter:area ratio

p1 <- ggplot(SW, aes(x = p_a_ratio, y = CO2_mean)) +
  geom_point(size=3) +
  geom_smooth(method = "lm") +
  labs(x = "", y = CO2_lab, tag = "b)") +
  theme

p1

model <- summary(lm(CO2_mean~p_a_ratio, SW))

p_values <- coef(model)[, "Pr(>|t|)"]
p_values

adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
adjusted_p_values

ggsave("Graphs/CO2 conc vs p_a.jpg")

p2 <- ggplot(SW, aes(x = p_a_ratio, y = CH4_mean)) +
  geom_point(size= 3) +
  geom_smooth(method = "lm") +
  labs(x = "Perimeter:area ratio", y = CH4_lab, tag = "e)") +
  theme
p2

model <- summary(lm(CH4_mean~p_a_ratio, SW))

p_values <- coef(model)[, "Pr(>|t|)"]
p_values

adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
adjusted_p_values

ggsave("Graphs/CH4 conc vs p_a.jpg")

p <- plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1))
p


# Mean concentrations with HAND 

h1 <- ggplot(SW, aes(x = hand_m, y = CO2_mean)) +
  geom_point(size=3) +
  geom_smooth(method = "lm") +
  theme +
  labs(x = "", y = CO2_lab, tag = "c)")

h1

model <- summary(lm(CO2_mean~hand_m, SW))

p_values <- coef(model)[, "Pr(>|t|)"]
p_values

adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
adjusted_p_values

ggsave("Graphs/CO2 vs HAND.jpg")


h2 <- ggplot(SW, aes(x = hand_m, y = CH4_mean)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme +
  labs(x = "HAND (m)", y = CH4_lab, tag = "f)")

h2

model <- summary(lm(CH4_mean~hand_m, SW))

p_values <- coef(model)[, "Pr(>|t|)"]
p_values

adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
adjusted_p_values

ggsave("Graphs/CH4 vs HAND.jpg")

h <- plot_grid(h1, h2, ncol = 1, align = "v", rel_heights = c(1, 1))
h

c <- plot_grid(a, p, h, ncol = 3)
c

ggsave("Graphs/MS/Morph figure.jpg", width = 14, height = 6)



ggplot(SW, aes(x = watershed, y = CO2_uM)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Wetland complex", y = CO2_lab) +
  theme


ggplot(CV_SW, aes(x = wet_order, y = CO2_mean)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_x_continuous(breaks=seq(0, 4, by = 1)) +
  labs(x = "Wetland order", y = CO2_lab) +
  theme

ggsave("Graphs/CO2 conc vs order.jpg")

ggplot(CV_SW, aes(x = wet_order, y = CH4_mean)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_x_continuous(breaks=seq(0, 4, by = 1)) +
  labs(x = "Wetland order", y = CH4_lab) +
  theme

ggsave("Graphs/CH4 conc vs order.jpg")


# With canopy cover
# Better to look at during the same time period canopy cover was taken in?

ggplot(CV_SW, aes(x = mean_gap_frac, y = CO2_mean)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  labs(x = "Mean gap fraction", y = CO2_lab) +
  ylim(200, 500) +
  theme

ggsave("Graphs/CO2 conc vs canopy.jpg")

ggplot(CV_SW, aes(x = mean_gap_frac, y = CH4_mean)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  labs(x = "Mean gap fraction", y = CH4_lab) +
  theme

ggsave("Graphs/CH4 vs canopy.jpg")


#### Plotting wetland variables with CVs ####

p2 <- ggplot(CV_SW, aes(x = area_m2, y = CV_CO2, color = Site)) +
  geom_point(size = 5) +
  labs(x = bquote("Wetland area" ~(m^2)), y = CO2_CV_lab, tag = "b") +
  theme_bw() +
  scale_x_log10() +
  geom_label(aes(label=Site)) +
  theme +
  theme(legend.position = "none")

p2

ggsave("Graphs/CVs/CV CO2 vs area_clean.jpg")
summary(lm(CV_CO2 ~ area_m2, data = CV_SW))

ggplot(CV_SW, aes(x = wetland_volume, y = CV_CH4, color = Site)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "log-Wetland storage volume (m^3)", y = CH4_CV_lab) +
  xlim(4.5, NA) +
  theme(legend.position = "none") +
  theme

ggsave("Graphs/CVs/CV CH4 vs log-vol.jpg")

p3 <- ggplot(CV_SW, aes(x = area_m2, y = CV_CH4, color = Site)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 500, label.y = 50) +
  labs(x = bquote("Wetland area" ~(m^2)), y = CH4_CV_lab, tag = "c") +
  geom_label(aes(label=Site)) +
  theme_bw() +
  scale_x_log10() +
  theme +
  theme(legend.position = "none")

p3

ggsave("Graphs/CVs/CV CH4 vs area.jpg")
summary(lm(CV_CH4 ~ area_m2, data = CV_SW))

# Plotting CV graphs together

g <- grid.arrange(p1, arrangeGrob(p2, p3), ncol = 2)

ggsave("Graphs/MS/Figure 4.jpg", g)


ggplot(CV_SW, aes(x = p_a_ratio, y = CV_CO2)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Perimeter:area ratio", y = "CV of CO2 (%)") +
  theme

ggsave("Graphs/CVs/CV CO2 vs p_a.jpg")

ggplot(CV_SW, aes(x = p_a_ratio, y = CV_CH4)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  geom_smooth(method = "lm") +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Perimeter:area ratio", y = "CV of CH4 (%)") +
  theme

ggsave("Graphs/CVs/CH4 vs p_a.jpg")


ggplot(CV_SW, aes(x = wet_order, y = CV_CO2)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Wetland order", y = "CV of CO2 (%)") +
  theme

ggsave("Graphs/CVs/CV CO2 vs wetland order.jpg")

ggplot(CV_SW, aes(x = wet_order, y = CV_CH4)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Wetland order", y = "CV of CH4 (%)") +
  theme

ggsave("Graphs/CVs/CV CH4 vs wetland order.jpg")


ggplot(CV_SW, aes(x = mean_gap_frac, y = CV_CO2)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Mean gap fraction", y = "CV of CO2 (%)") +
  theme

ggsave("Graphs/CVs/CV CO2 vs canopy cover.jpg")

ggplot(CV_SW, aes(x = mean_gap_frac, y = CV_CH4)) +
  geom_point() +
  geom_label(aes(label=Site)) +
  scale_color_gradientn(colors=met.brewer("Egypt")) +
  labs(x = "Mean gap fraction", y = "CV of CH4 (%)") +
  theme

ggsave("Graphs/CVs/CV CH4 vs canopy cover.jpg")




#### Bonferroni corrections ####

CO2_models <- list(
  lm1 = lm(CO2_mean ~ area_m2, data = SW),
  lm2 = lm(CO2_mean ~ p_a_ratio, data = SW),
  lm3 = lm(CO2_mean ~ hand_m, data = SW)
)

# Initialize a list to store p-values
p_values_list <- list()

# Loop through each model to extract p-values
for (model_name in names(CO2_models)) {
  # Get the summary of the model
  summary_model <- summary(CO2_models[[model_name]])
  
  # Extract coefficients matrix
  coefficients_matrix <- summary_model$coefficients
  
  # Check if the coefficient of interest is present
  coefficient_name <- rownames(coefficients_matrix)[2]  # Coefficient of interest (excluding intercept)
  
  if (!is.na(coefficient_name) && coefficient_name != "(Intercept)") {
    # Extract p-value for the coefficient
    p_value <- coefficients_matrix[coefficient_name, "Pr(>|t|)"]
  } else {
    # If the coefficient is not present, assign NA
    p_value <- NA
    warning(paste("Coefficient not found in model", model_name))
  }
  
  # Store the p-value in the list with the model name
  p_values_list[[model_name]] <- p_value
}

view(p_values_list)

# Correct those p-values
# p.adjust uses the number of models to correct the alpha value

adjusted_p_values <- p.adjust(p_values_list, method = "bonferroni")
adjusted_p_values




CH4_models <- list(
  lm1 = lm(CH4_mean ~ area_m2, data = SW),
  lm2 = lm(CH4_mean ~ p_a_ratio, data = SW),
  lm3 = lm(CH4_mean ~ hand_m, data = SW)
)

# Initialize a list to store p-values
p_values_list <- list()

# Loop through each model to extract p-values
for (model_name in names(CH4_models)) {
  # Get the summary of the model
  summary_model <- summary(CH4_models[[model_name]])
  
  # Extract coefficients matrix
  coefficients_matrix <- summary_model$coefficients
  
  # Check if the coefficient of interest is present
  coefficient_name <- rownames(coefficients_matrix)[2]  # Coefficient of interest (excluding intercept)
  
  if (!is.na(coefficient_name) && coefficient_name != "(Intercept)") {
    # Extract p-value for the coefficient
    p_value <- coefficients_matrix[coefficient_name, "Pr(>|t|)"]
  } else {
    # If the coefficient is not present, assign NA
    p_value <- NA
    warning(paste("Coefficient not found in model", model_name))
  }
  
  # Store the p-value in the list with the model name
  p_values_list[[model_name]] <- p_value
}

p_values_list

# Correct those p-values
# p.adjust uses the number of models to correct the alpha value

adjusted_p_values <- p.adjust(p_values_list, method = "bonferroni")
adjusted_p_values
