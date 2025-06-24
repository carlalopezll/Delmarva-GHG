# Script for plotting water level at Jackson Lane
# Carla López Lloreda
# Last updated 10/23/2024

library(purrr)
library(lubridate)
library(scales)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Read in data

wl <- read_csv("Hydrology/data/WL_JL_250403.csv")

#### Plotting ####

# Theme stuff
theme <- theme(axis.text = element_text(size = "16"), axis.title = element_text(size=30))

color_palette_wl <- c("DK-SW" = "#F8766D", 
                   "ND-SW" = "#00BA38", 
                   "TS-SW" = "#619CFF")

#### Water level for sensor data ####

# Plotting on the same time frame as CO2 sensor data

lims <- as.POSIXct(strptime(c("2021-04-14 12:30:00", "2024-10-04 00:00"),
                            format = "%Y-%m-%d %H:%M"))

lims <- as.POSIXct(strptime(c("2021-04-14 12:30:00", "2022-08-04 00:00"),
                            format = "%Y-%m-%d %H:%M"))

# Blank graph for presentation
ggplot(wl)+
  labs(x = "", y = "Water level (m)") +
  scale_x_datetime(limits = lims, labels = date_format("%Y-%m"), date_breaks = "3 months") +
  ylim(-0.2,1.3) +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() +
  theme +
  # Diagonal year breaks
  geom_segment(
    data = diag_lines,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "black"
  ) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggsave("Hydrology/graphs/WL blank.jpg", width = 18, height = 6, units= "in")

# Create vector of year breaks based on your data
year_breaks <- seq(
  from = as.POSIXct(format(min(wl$Timestamp_corrected, na.rm = TRUE), "%Y-01-01")),
  to   = as.POSIXct(format(max(wl$Timestamp_corrected, na.rm = TRUE), "%Y-01-01")),
  by = "1 year"
)

# Get y-axis range for the diagonal lines
ymin <- min(wl$waterLevel, na.rm = TRUE)
ymax <- max(wl$waterLevel, na.rm = TRUE)

# Create dataframe of diagonal segments
diag_lines <- data.frame(
  x = year_breaks,
  xend = year_breaks + 3600 * 24,  # small shift for diagonal (1 day)
  y = ymin,
  yend = ymax
)

ggplot(wl, aes(x = Timestamp_corrected, y = waterLevel, color = `Site_Name`)) +
  labs(x = "", y = "Water level (m)") +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months", limits = lims) +
  geom_hline(yintercept = 0, size = 2) +
  # Diagonal year breaks
  geom_segment(
    data = diag_lines,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "black"
  ) +
  theme_bw() +
  theme +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggsave("Hydrology/graphs/WL blank.jpg", width = 18, height = 6, units= "in")

# Plot
ggplot(wl, aes(x = Timestamp_corrected, y = waterLevel, color = `Site_Name`)) +
  geom_point(size = 0.8, na.rm = TRUE) + 
  labs(x = "", y = "Water level (m)") +
  scale_x_datetime(labels = date_format("%Y-%m"), date_breaks = "3 months", limits = lims) +
  geom_hline(yintercept = 0, size = 2) +
  # Diagonal year breaks
  geom_segment(
    data = diag_lines,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "black"
  ) +
  theme_bw() +
  theme +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggsave("Hydrology/graphs/JL WL.jpg", width = 18, height = 6, units= "in")

# Plotting by year
clean_wl <- clean_wl %>%
  mutate(Year = year(Timestamp_corrected)) %>%
  group_by(Year) %>%
  mutate(Start_of_Year = as.POSIXct(paste(Year, "01", "01", sep = "-")),
         End_of_Year = as.POSIXct(paste(Year, "12", "31", sep = "-"))) %>%
  ungroup()

# Create a list to store plots
plot_list <- list()

# Generate plots for each year with specific limits
for (year in unique(clean_wl$Year)) {
  start_date <- as.POSIXct(paste(year, "01", "01", sep = "-"))
  end_date <- as.POSIXct(paste(year, "12", "31", sep = "-"))
  
  p <- ggplot(filter(clean_wl, Year == year), aes(x = Timestamp_corrected, y = waterLevel, color = Site_Name)) +
    geom_line() +
    theme_bw() +
    labs(x = "", y = "Water level (m)") +
    scale_x_datetime(limits = c(start_date, end_date),
                     breaks = seq(from = start_date, to = end_date, by = "2 months"),
                     labels = date_format("%Y-%m"),
                     expand = expansion(mult = c(0, 0))) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    theme(legend.position = "right") +
    scale_color_manual(values = color_palette_wl) +
    ggtitle(paste("", year))
  
  plot_list[[as.character(year)]] <- p
}

# Display all plots in a grid
wl_graph <- do.call(grid.arrange, c(plot_list, ncol = 1))

ggsave(plot = wl_graph, "Hydrology/graphs/WL timeseries by year.jpg", width = 8, height = 8, units = "in")
