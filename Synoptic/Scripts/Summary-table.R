library(tidyverse)
library(dplyr)
library(confintr)

detach(package:plyr)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

#### Cleaning and merging data ####

# Read in merged (includes all analytes, GHG, field data, water level, and watershed characteristics)
merge <- read.csv("Master spreadsheet.csv")

# Change the low variables to ug/L

merge$TDN_ugL <- merge$TDN*1000
merge$NO3_ugL <- merge$NO3*1000
merge$NH4_ugL <- merge$NH3*1000
merge$TDP_ugL <- merge$TDP*1000

# Subset for SW 

SW <- merge %>%
  filter(Sample_Type == "SW") %>%
  filter(Site_dry == "No") %>%
  filter(!(Site == "AG") & !(Site == "TR") & !(Site == "CR")) %>%
  filter(CH4_uM > 0)

# Making a table of averages, ranges, and standard deviations
# How to get a mean per site for all the variables? + include all data info

site_avg_SW <- SW %>%
  group_by(Site_ID) %>%
  summarise(Sample_Type = first(Sample_Type), 
            Site = first(Site),
            New_Site = first(Site_new),
            avg_CO2 = mean(CO2_uM),
            std_CO2 = sd(CO2_uM),
            min_CO2 = min(CO2_uM),
            max_CO2 = max(CO2_uM),
            avg_CH4 = mean(CH4_uM),
            std_CH4 = sd(CH4_uM),
            min_CH4 = min(CH4_uM),
            max_CH4 = max(CH4_uM))

# Format previous table to have avg, min, and max in parentheses
site_avg_SW <- site_avg_SW %>%
  mutate(avg_CO2_formatted = sprintf("%.1f", avg_CO2),
                      range_CO2_formatted = sprintf("(%s - %s)", 
                                                    signif(min_CO2, digits = 6),
                                                    signif(max_CO2, digits = 6)),
                      sd_CO2_formatted = sprintf("%.1f", std_CO2),
                      avg_and_range_CO2 = paste(avg_CO2_formatted, "+-", sd_CO2_formatted, range_CO2_formatted, sep = " "),
                      avg_CH4_formatted = sprintf("%.1f", avg_CH4),
                      range_CH4_formatted = sprintf("(%s - %s)", 
                                                    signif(min_CH4, digits = 6),
                                                    signif(max_CH4, digits = 6)),
                      sd_CH4_formatted = sprintf("%.1f", std_CH4),
                      avg_and_range_CH4 = paste(avg_CH4_formatted, "+-", sd_CH4_formatted, range_CH4_formatted, sep = " "))



# Function to calculate formatted statistics

## With ranges
# calculate_stats <- function(x) {
#   mean_val <- round(mean(x, na.rm = TRUE), 1)
#   sd_val <- round(sd(x, na.rm = TRUE), 1)
#   min_val <- round(min(x, na.rm = TRUE), 1)
#   max_val <- round(max(x, na.rm = TRUE), 1)
#   return(paste0(mean_val, " ± ", sd_val, " (", min_val, "-", max_val, ")"))
# }

# Without ranges
calculate_stats <- function(x) {
  mean_val <- round(mean(x, na.rm = TRUE), 1)
  sd_val <- round(sd(x, na.rm = TRUE), 1)
  return(paste0(mean_val, " ± ", sd_val))
}


# Function to perform Tukey HSD and extract letters
get_tukey_letters <- function(var, data) {
  # Perform ANOVA
  aov_result <- aov(as.formula(paste(var, "~ Site_new")), data = data)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(aov_result)
  
  # Extract Tukey letters from Tukey HSD results
  tukey_letters <- multcompLetters4(aov_result, tukey_result)
  
  # Convert Tukey letters to a data frame
  tukey_df <- data.frame(
    Site_new = names(tukey_letters$`Site_new`$Letters),
    Letters = as.character(tukey_letters$`Site_new`$Letters),
    stringsAsFactors = FALSE
  )
  
  return(tukey_df)
}

# List of variables to analyze
variables <- c("CO2_uM", "CH4_uM", "Temp_C", "pH", "SpC", "DO_mgL", "NPOC", "TDP_ugL", "TDN_ugL", "NH4_ugL", "NO3_ugL")
variable_labels <- c("CO2 (uM)", "CH4 (uM)", "Temperature (C)", "pH", "Specific Conductivity (uS/cm)", "DO (mg/L)", "DOC (mg/L)", "TDP (ug/L)", "TDN (ug/L)", "NH4 (ug/L)", "NO3 (ug/L)")

# Create an empty list to store the summary tables
summary_list <- list()

# Loop through each variable
for (i in seq_along(variables)) {
  var <- variables[i]
  var_label <- variable_labels[i]
  
  # Create a summary table for the current variable
  summary_table <- SW %>%
    group_by(Site_new) %>%
    summarise(
      `Wetland complex` = first(watershed),
      Stats = calculate_stats(.[[var]])
    ) %>%
    ungroup()
  
  # Get Tukey letters for the current variable
  tukey_df <- get_tukey_letters(var, SW)
  
  # Merge Tukey letters with the summary table
  summary_table <- summary_table %>%
    left_join(tukey_df, by = "Site_new") %>%
    mutate(Stats = paste0(Stats, " ", Letters)) %>%
    select(Site_new, `Wetland complex`, Stats)  # Select relevant columns
  
  # Add a variable label for clarity
  summary_table$Variable <- var_label
  
  # Append to the list
  summary_list[[var]] <- summary_table
}

# Combine all summary tables into one data frame
final_summary_table <- bind_rows(summary_list)

full_chem_table <- SW %>%
  group_by(Site_new) %>%
  summarise(
    "Wetland complex" =  first(watershed),
    "CO2 (uM)" = calculate_stats(CO2_uM),
    "CH4 (uM)" = calculate_stats(CH4_uM),
    "Temperature (C)" = calculate_stats(Temp_C),
    pH = calculate_stats(pH),
    "Specific Conductivity (uS/cm)" = calculate_stats(SpC),
    "DO (mg/L)" = calculate_stats(DO_mgL),
    "DOC (mg/L)" = calculate_stats(NPOC),
    "TDP (ug/L)" = calculate_stats(TDP_ugL),
    "TDN (ug/L)" = calculate_stats(TDN_ugL),
    "NH4 (ug/L)"= calculate_stats(NH4_ugL),
    "NO3 (ug/L)" = calculate_stats(NO3_ugL)
  )


write_csv(full_chem_table, "Water chem and GHG table_no range.csv")




library(multcompView)
library(agricolae)

# Function to perform Tukey's HSD and return group letters
tukey_letters <- function(model) {
  tukey <- HSD.test(model, "Site_new")
  return(as.data.frame(tukey$groups)$groups)
}

# Perform ANOVA and Tukey's HSD test, then extract group letters for each parameter
anova_tukey <- function(data, param) {
  model <- aov(data[[param]] ~ Site_new, data = data)
  return(tukey_letters(model))
}

# Perform Tukey's HSD test for each parameter
tukey_results <- data.frame(
  Site_new = unique(SW$Site_new),
  CO2_letter = anova_tukey(SW, "CO2_uM"),
  CH4_letter = anova_tukey(SW, "CH4_uM"),
  Temp_letter = anova_tukey(SW, "Temp_C"),
  pH_letter = anova_tukey(SW, "pH"),
  Conductivity_letter = anova_tukey(SW, "SpC"),
  DO_letter = anova_tukey(SW, "DO_mgL"),
  TDN_letter = anova_tukey(SW, "TDN"),
  TDP_letter = anova_tukey(SW, "TDP"),
  DOC_letter = anova_tukey(SW, "NPOC"),
  NH4_letter = anova_tukey(SW, "NH4_ugL"),
  NO3_letter = anova_tukey(SW, "NO3_ugL")
  
)

# Calculate the statistics per site and add Tukey's HSD letters
stats_summary_per_site <- SW %>%
  group_by(Site_new) %>%
  summarise(
    "Wetland complex" = first(watershed),
    CO2 = calculate_stats(CO2_uM),
    CH4 = calculate_stats(CH4_uM),
    Temp = calculate_stats(Temp_C),
    pH = calculate_stats(pH),
    Conductivity = calculate_stats(SpC),
    DO = calculate_stats(DO_mgL),
    DOC = calculate_stats(NPOC),
    TDP = calculate_stats(TDP_ugL),
    TDN = calculate_stats(TDN_ugL),
    NH4 = calculate_stats(NH4_ugL),
    NO3 = calculate_stats(NO3_ugL)
  ) %>%
  left_join(tukey_results, by = "Site_new") %>%
  mutate(
    CO2 = paste0(CO2, tukey_results$CO2_letter[match(Site_new, tukey_results$Site_new)]),
    CH4 = paste0(CH4, tukey_results$CH4_letter[match(Site_new, tukey_results$Site_new)]),
    Temp = paste0(Temp, tukey_results$Temp_letter[match(Site_new, tukey_results$Site_new)]),
    pH = paste0(pH, tukey_results$pH_letter[match(Site_new, tukey_results$Site_new)]),
    Conductivity = paste0(Conductivity, tukey_results$Conductivity_letter[match(Site_new, tukey_results$Site_new)]),
    DO = paste0(DO, tukey_results$DO_letter[match(Site_new, tukey_results$Site_new)]),
    DOC = paste0(DOC, tukey_results$DOC_letter[match(Site_new, tukey_results$Site_new)]),
    TDP = paste0(TDP, tukey_results$TDP_letter[match(Site_new, tukey_results$Site_new)]),
    TDN = paste0(TDN, tukey_results$TDN_letter[match(Site_new, tukey_results$Site_new)]),
    NH4 = paste0(NH4, tukey_results$NH4_letter[match(Site_new, tukey_results$Site_new)]),
    NO3 = paste0(NO3, tukey_results$NO3_letter[match(Site_new, tukey_results$Site_new)]),
  )

write_csv(stats_summary_per_site, "Water chem and GHG table w Tukey.csv")




# Function to perform Tukey's HSD and return group letters for a given parameter
tukey_letters <- function(data, param) {
  model <- aov(data[[param]] ~ Site, data = data)
  tukey <- HSD.test(model, "Site")
  letters_df <- data.frame(Site = names(tukey$groups), Letter = tukey$groups$groups)
  return(letters_df)
}

# Function to perform Tukey's HSD for multiple parameters and return a list of data frames with letters
get_tukey_letters_for_params <- function(data, params) {
  letters_list <- list()
  for (param in params) {
    letters_df <- tukey_letters(data, param)
    letters_list[[param]] <- letters_df
  }
  return(letters_list)
}

# Function to plot a parameter with Tukey's letters
plot_with_tukey_letters <- function(data, param, tukey_letters_df) {
  ggplot(data, aes(x = Site, y = .data[[param]], fill = Site)) +
    geom_boxplot() +
    geom_text(data = tukey_letters_df, aes(label = Letter, y = max(data[[param]], na.rm = TRUE) * 1.05), size = 5) +
    labs(title = paste("Boxplot of", param, "with Tukey's HSD Letters"), y = param) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


