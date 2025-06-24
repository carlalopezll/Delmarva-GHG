
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
library(plotly)
library(readr)
library(gridExtra) # for merging plots

# For Tukey
library(emmeans)
library(multcompView)
library(multcomp)


setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Axis titles for subscripts in CH4 and CO2
CO2_lab <- expression(paste("C","O"[2]^{}*" ("*mu,"M)"))
CH4_lab <- expression(paste("C","H"[4]^{}*" ("*mu,"M)"))

# Theme stuff
theme <- theme_bw() +
  theme(text = element_text(size = 20))
# theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                    axis.line = element_line(colour = "black"))


# Read in merged (includes all analytes, GHG, field data, water level, and watershed characteristics)
merge <- read_csv("data/Master spreadsheet.csv")

# Rearrange factor order by site/watershed

merge$Site <- factor(merge$Site,levels = c("ND", "BD", "TS", "DK", "FR", # Jackson Lane
                                           "TA", "TB", "DB", "FN", # Tiger Paw/Beetree Rd (Jackson Lane)
                                           "JA", "JB", "JC", "NB", # Jones Rd N (Baltimore Corner)
                                           "OB", "XB", "MB", "HB", "TP", # Baltimore Corner
                                           "TI", "QB", "DF", # Jones Rd S (Baltimore Corner)
                                           "CR", "TR", "AG")) # Rivers, own name for watershed

merge$watershed <- ifelse(merge$Site == "ND" | merge$Site == "BD" |
                            merge$Site == "TS" | merge$Site == "DK" | merge$Site == "FR", merge$watershed <- "Jackson Lane", 
                          ifelse(merge$Site == "TA" | merge$Site == "TB" | merge$Site == "DB" |
                                   merge$Site == "FN", merge$watershed <- "Tiger Paw",
                                 ifelse(merge$Site == "JA" | merge$Site == "JB" |
                                          merge$Site == "JC" | merge$Site == "NB", merge$watershed <- "Jones Road" , 
                                        ifelse(merge$Site == "OB" | merge$Site == "XB" |
                                                 merge$Site == "MB" | merge$Site == "HB", merge$watershed <- "Baltimore Corner", 
                                               ifelse(merge$Site == "TI" | merge$Site == "QB" | merge$Site == "DF", merge$watershed <- "Baltimore Corner", NA)))))

# Subset data

SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  filter(Site_dry == "No") %>%
  filter(!(Site == "AG") & !(Site == "TR") & !(Site == "CR"))

JL <- SW %>%
  filter(watershed == "Jackson Lane") %>%
  filter(!(Site == "FR"))

JL$Site <- factor(JL$Site, levels = c("BD", "ND", "TS", "DK"))

ggplot(JL, aes(x= Site, y = CO2_uM, fill = Site)) +
  geom_boxplot() +
  geom_jitter(width = 0)

ggsave("graphs/JL CO2 boxplot.jpg")

#### Boxplots across sites ####

## Grouping by "watersheds"
## Groups are: Jackson Lane, Beetree Road, Baltimore Corner, Jones Road

p1 <- ggplot(SW, aes(x = Site_new, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, fill = "Wetland complex", tag = "a)") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=18), axis.text = element_text(size = 16)) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  ) +
  theme +
  scale_y_log10()

p1

ggsave("Graphs/Boxplots/CO2_SW_boxplot.jpg")


p2 <- ggplot(SW, aes(x = Site_new, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme
# theme(
#   legend.position = c(.99, .99),
#   legend.justification = c("right", "top"),
#   legend.box.just = "right",
#   legend.margin = margin(6, 6, 6, 6),
#   axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
# )

p2

ggsave("Graphs/Boxplots/CH4_SW_boxplot.jpg")

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3 alternative.jpg", g)



# Tukey test on CO2 across sites
ANOVA_CO2 <- aov(SW$CO2_uM~SW$Site_new)
summary(ANOVA_CO2)
TUKEY_CO2 <- TukeyHSD(ANOVA_CO2)
cld_CO2 <- multcompLetters4(ANOVA_CO2, TUKEY_CO2)
dt_CO2 <- SW %>% 
  group_by(Site_new) %>%
  summarise(w = mean(CO2_uM), sd = sd(CO2_uM)) %>%
  arrange(desc(w))
cld_CO2 <- as.data.frame.list(cld_CO2$`SW$Site_new`)
dt_CO2$cld <- cld_CO2$Letters
dt_CO2$CO2_uM <- dt_CO2$w

# ANOVA for CO2
summary(ANOVA_CO2)
print(TUKEY_CO2)

# CO2 across sites with Tukey

p1 <- ggplot(NULL, aes(x = reorder(Site_new, -CO2_uM, mean), y = CO2_uM)) + 
  geom_boxplot(data = SW, aes(x = reorder(Site_new, -CO2_uM, mean), y = CO2_uM, fill = watershed)) + 
  geom_jitter(data = SW, width = 0.1) +
  labs(x = "Site", y = CO2_lab, tag = "a)", fill = "Wetland complex") +
  theme +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=20), axis.text = element_text(size = 20)) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  geom_label(data = dt_CO2, aes(x = Site_new, y = w + sd + 150, label = cld), size = 6, alpha = 1)

p1


# Tukey test on CH4 across sites

ANOVA_CH4 <- aov(SW$CH4_uM~SW$Site_new)
TUKEY_CH4 <- TukeyHSD(ANOVA_CH4)
cld_CH4 <- multcompLetters4(ANOVA_CH4, TUKEY_CH4)
dt_CH4 <- SW %>% 
  group_by(Site_new) %>%
  summarise(w = mean(CH4_uM), sd = sd(CH4_uM)) %>%
  arrange(desc(w))
cld_CH4 <- as.data.frame.list(cld_CH4$`SW$Site_new`)
dt_CH4$cld <- cld_CH4$Letters
dt_CH4$CH4_uM <- dt_CH4$w

summary(ANOVA_CH4)
print(TUKEY_CH4)


# ggplot(NULL, aes(x = reorder(Site, -CH4_uM, mean), y = CH4_uM)) + 
#   geom_boxplot(data = SW, aes(fill = wetland_complex)) + 
#   geom_jitter(data = SW) +
#   geom_text(data = dt, aes(label = cld, y = w + sd + 5), vjust = -0.5, colour = 'white') +
#   labs(x = "Site", y = CH4_lab, tag = "b)") +
#   theme

p2 <- ggplot(NULL, aes(x = reorder(Site_new, -CH4_uM, mean), y = CH4_uM)) + 
  geom_boxplot(data = SW, aes(x = reorder(Site_new, -CH4_uM, mean), y = CH4_uM, fill = watershed)) + 
  geom_jitter(data = SW, width = 0.1) + 
  labs(x = "Site", y = CH4_lab, tag = "b)", fill = "Wetland complex") +
  scale_y_log10() +
  geom_label(data = dt_CH4, aes(x = Site_new, y = w + sd + 100, label = cld), size = 6, alpha= 1)  +
  theme +
  theme(axis.text.x = element_text(size = "20"), axis.title = element_text(size = 20), legend.position = "none")

p2

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3_Tukey.jpg", g, width = 14, height = 8, units = "in", dpi = 300)

# Plotting across wetland complexes

ANOVA_CO2_complex <- aov(SW$CO2_uM~SW$watershed)
TUKEY_CO2_complex <- TukeyHSD(ANOVA_CO2_complex)
cld_CO2_complex <- multcompLetters4(ANOVA_CO2_complex, TUKEY_CO2_complex)
dt_CO2_complex <- SW %>% 
  group_by(watershed) %>%
  summarise(w = mean(CO2_uM), sd = sd(CO2_uM)) %>%
  arrange(desc(w))
cld_CO2_complex <- as.data.frame.list(cld_CO2_complex$`SW$Site`)
dt_CO2_complex$cld <- cld_CO2_complex$Letters
dt_CO2_complex$CO2_uM <- dt_CO2_complex$w

summary(ANOVA_CO2_complex)
print(TUKEY_CO2_complex)


c1 <- ggplot(SW, aes(x = watershed, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, tag = "a)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme



ANOVA_CH4_complex <- aov(SW$CH4_uM~SW$watershed)
TUKEY_CH4_complex <- TukeyHSD(ANOVA_CH4_complex)
cld_CH4_complex <- multcompLetters4(ANOVA_CH4_complex, TUKEY_CH4_complex)
dt_CH4_complex <- SW %>% 
  group_by(watershed) %>%
  summarise(w = mean(CH4_uM), sd = sd(CH4_uM)) %>%
  arrange(desc(w))
cld_CH4_complex <- as.data.frame.list(cld_CH4_complex$`SW$Site`)
dt_CH4_complex$cld <- cld_CH4_complex$Letters
dt_CH4_complex$CH4_uM <- dt_CH4_complex$w

summary(ANOVA_CH4_complex)
print(TUKEY_CH4_complex)


c2 <- ggplot(SW, aes(x = watershed, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b)", fill = "Wetland complex") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme

c <- plot_grid(c1, c2, ncol= 1)
c

ggsave("Graphs/MS/wetland complex.jpg")


p1 <- ggplot(SW, aes(x = Site_new, y = CO2_uM, fill = wetland_complex)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CO2_lab, fill = "Wetland complex", tag = "a") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title = element_text(size=18), axis.text = element_text(size = 16)) +
  theme(
    legend.position = c(.90, .30),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  ) +
  theme +
  scale_y_log10()
p1

ggsave("Graphs/Boxplots/CO2_SW_boxplot.jpg")

p2 <- ggplot(SW, aes(x = Site_new, y = CH4_uM, fill = wetland_complex)) +
  geom_boxplot() +
  geom_jitter(width = 0) +
  labs(x = "Site", y = CH4_lab, tag = "b") +
  theme_bw() +
  theme(axis.title = element_text(size=18), axis.text = element_text(size = 16),
        legend.position = "none") +
  theme +
  scale_y_log10()
# theme(
#   legend.position = c(.99, .99),
#   legend.justification = c("right", "top"),
#   legend.box.just = "right",
#   legend.margin = margin(6, 6, 6, 6),
#   axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
# )
p2

ggsave("Graphs/Boxplots/CH4_SW_boxplot.jpg")

g <- grid.arrange(grobs = list(p1, p2), widths = c(1, 1), layout_matrix = rbind(c(1, 1), c(2, 2)))

ggsave("Graphs/MS/Figure 3.jpg", g)




# For groundwater

ggplot(UW, aes(x = Site, y = CH4_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Site", y = CH4_lab) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CH4_UW_boxplot.jpg")

ggplot(UW, aes(x = Site, y = CO2_uM, fill = watershed)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Site", y = CO2_lab) +
  theme(
    legend.position = c(.86, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_text(size = "12"), axis.title = element_text(size = 15)
  )

ggsave("Graphs/CO2_UW_boxplot.jpg")




# Creating well number column

all_UW$well <- as.numeric(substr(all_UW [ , 1], start= 9, stop= 9))

# Looking at CO2-DOC residuals

CO2_DOC <- lm(CO2_uM ~ NPOC_mgC_L, data = all_UW)
ols_plot_resid_fit(CO2_DOC)

all_UW$residuals <- CO2_DOC$residuals

ggplot(all_UW, aes(x = Site_ID, y = residuals)) +
  geom_point() +
  labs(x = "Well position", y = "Residuals") +
  theme

ggsave("Graphs/residuals CO2 vs DOC_all.jpg")


# This is pretty cool:
ggstatsplot::ggscatterstats(data = SW, x = NPOC, y = CO2_uM)




# Plotting all of the other variables

# List of variables to analyze
variables <- c('Temp_C', 'pH', 'NPOC', 'NH3', 'NO3', 'TDN', 'TDP')

# Loop through each variable
for (var in variables) {
  # Check if the variable exists in the DataFrame and if 'Site' and 'Watershed' columns exist
  if (var %in% names(SW) && 'Site_new' %in% names(SW) && 'watershed' %in% names(SW)) {
    # Perform ANOVA
    aov_result <- aov(as.formula(paste(var, "~ Site_new")), data = SW)
    
    # Perform Tukey's HSD test
    tukey_result <- TukeyHSD(aov_result)
    
    # Extract Tukey letters
    tukey_letters <- as.data.frame(tukey_result$`Site_new`)
    tukey_letters$Site_new <- rownames(tukey_letters)
    tukey_letters <- tukey_letters %>%
      select(Site_new, letters = `letters`)
    
    # Create a new data frame with Tukey letters for annotation
    data <- merge(data, tukey_letters, by.x = 'Site_new', by.y = 'Site_new')
    
    # Create a boxplot for the variable, colored by Watershed
    p <- ggplot(SW, aes_string(x = 'Site_new', y = var, fill = 'watershed')) +
      geom_boxplot() +
      geom_jitter(width = 0) +
      geom_text(data = tukey_letters, aes(x = Site_new, y = Inf, label = letters), vjust = -0.5, hjust = 0.5) +
      xlab('Site') +
      ylab(var) +
      theme
    
    # Print the plot
    print(p)
    
  } else {
    cat(sprintf("Variable %s, 'Site' column, or 'Watershed' column not found in the DataFrame.\n", var))
  }
}



# Create a function to perform Tukey HSD and generate boxplot with letters
plot_variable <- function(var, data) {
  # Perform ANOVA
  aov_result <- aov(as.formula(paste(var, "~ Site_new")), data = data)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(aov_result)
  
  # Extract Tukey letters from Tukey HSD results
  tukey_letters <- multcompLetters4(aov_result, tukey_result)
  
  # Convert Tukey letters to a data frame
  tukey_df <- as.data.frame(tukey_letters$`Site_new`)
  tukey_df$Site_new <- rownames(tukey_df)
  colnames(tukey_df) <- c("Letters", "Site_new")
  
  # Merge Tukey letters with original data
  data_with_letters <- merge(data, tukey_df %>% dplyr::select(Site_new, Letters), by.x = 'Site_new', by.y = 'Site_new')
  
  # Check for missing values in Tukey letters
  if (any(is.na(data_with_letters$Letters))) {
    warning("Tukey letters contain NA values after merging. Please check.")
  }
  
  # Merge Tukey letters with original data
  data <- merge(data, tukey_df %>% dplyr::select(Site_new, Letters), by.x = 'Site_new', by.y = 'Site_new')
  
  # Determine y-position for text annotation
  max_y <- max(data[[var]], na.rm = TRUE)
  y_offset <- max_y * 0.05  # 5% above the maximum y value
  
  # Create the boxplot
  p <- ggplot(data, aes(x = Site_new, y = .data[[var]], fill = watershed)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +  # Add jitter for individual points
    geom_text(aes(label = Letters), position = position_nudge(y = y_offset), size = 3) +
    xlab('Site') +
    ylab(var) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save the plot as a JPG file
  ggsave(filename = paste0(var, "_boxplot.jpg"), plot = p, width = 8, height = 6, units = "in", dpi = 300)
}

# Loop through each variable and create plots
for (var in variables) {
  if (var %in% names(SW) && 'Site_new' %in% names(SW) && 'watershed' %in% names(SW)) {
    plot_variable(var, SW)
  } else {
    cat(sprintf("Variable %s, 'Site_new' column, or 'watershed' column not found in the DataFrame.\n", var))
  }
}
