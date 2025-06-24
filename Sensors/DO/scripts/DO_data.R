# Script for looking at dissolved oxygen data
# Code created by: Carla Lopez Lloreda
# Last updated: 4/5/2023
# Important note: this data stil needs to be corrected for:
#   (1) Winkler/bucket calibrations
#   (2) Atmospheric pressure

# Install packages
# If you don't have these, install them from packages
library(ggplot2) # for plotting
library(dplyr) # for manipulating data
library(lubridate) # for fixing datetimes

# Read in data

do <- read_csv("DO/raw data/DO_JL_Raw.csv")

# Fix time-stamp since it currently recognizes it as a character
# We will use the as_datetime function from the lubridate package
# do$Timestamp <- do$Eastern_Standard_Time

# Look at general stats

summary(do)

lims <- as.POSIXct(strptime(c("2021-04-14 12:30:00", "2024-10-04 00:00"),
                            format = "%Y-%m-%d %H:%M"))

# Plot data over time
g <- ggplot(do, aes(x= Timestamp, y = DO_conc_mgL, color = Site_ID)) +
  geom_line() +
  scale_x_datetime(limits = lims, labels = date_format("%Y-%m"), date_breaks = "4 months") +
  labs(x= "", y = "DO (mg/L)") +
  theme

g

ggsave("DO/graphs/DO timeseries.jpg")

ggplot(do, aes(x= Logger_TempC, y= DO_conc_mgL)) +
  geom_point()

# Identify outliers
  
# Outliers using boxplots
ggplot(do, aes(x = "", y= DO_mgL)) +
  geom_boxplot()

# Outliers using histograms
ggplot(do, aes(x= DO_mgL)) +
    geom_histogram()

# This piece of code identifies outliers and creates a new dataframe for them so we can take a better look at them

out <- boxplot.stats(do$DO_mgL)$out
out_ind <- which(do$DO_mgL %in% c(out))
out_ind

outliers <- do[out_ind, ]

# Here you can look at the outliers. If you arrange them from low to high for DO_mgL, you can see
# that there's a big jump in DO_perc and temperature between 7.322 and 7.595mg/L of DO_mgL!
# Which to me suggests that anything above 7.595mg/L means it's out of water (high percent and high temp)
View(outliers)

# Remove outliers
# For DO concentrations, we will use anything >7.5mg/L as our threshold for outliers
# We will use the filter function from the dplyr package for this

do_clean <- filter(do, DO_mgL < 7.5) # replace this with NA instead of removing

# Replace removed rows NAs

# Raw data plotted over time + filtered/corrected data

# Now plot again!

ggplot(do_clean, aes(x= Timestamp, y = DO_mgL)) +
  geom_point()

# ggsave("DK_DO.jpg")

# This was only for DO concentrations (mg/L)
# You can now plot temp, Q, battery and DO saturation (%)
# No need to do the outlier process again since we already used DO concentrations to do that

ggplot(do_clean, aes(x= Timestamp, y = DO_mgL)) +
  geom_point()

ggplot(do_clean, aes(x= Timestamp, y= Q)) +
  geom_point()

ggplot(do_clean, aes(x= Timestamp, y = Battery_V)) +
  geom_point()

ggplot(do, aes(x= Temp_C, y = DO_Concentration_mgL, color = Timestamp)) +
  geom_point() +
  guides(colour = guide_colourbar(reverse = TRUE))

ggplot(do, aes(x= Temp_C, y = DO_Concentration_mgL, color = Site_ID)) +
  geom_point()

scale_fill_continuous(breaks = c(1000, 2000, 4000))


write.csv(summary_DO, "Summary_DO.csv")
