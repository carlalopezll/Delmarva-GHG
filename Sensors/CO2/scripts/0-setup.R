# Set up files for consistency across scripts

# Load libraries

library(ggplot2)
library(readr)
library(tidyverse)
library(lubridate)

# Read in data that they all use
co2 <- read_csv("CO2/processed data/eosGP_JL_clean.csv")

# Theme
theme <- theme_bw() + theme(axis.text = element_text(size = "16"), axis.title = element_text(size=20))

