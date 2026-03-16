# load libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(padr)

# load wetland metabolism functions first
source("scripts/01-wetland metab functions.R")

# read in data
do <- read_csv("../Sensors/DO/merged data/DO_JL_Raw.csv") %>%
  select(-file, -BattV)

# fixing TS time-intervals
do <- do %>%
  arrange(Timestamp) %>%
  
  # Snap each timestamp to nearest 15-min boundary
  thicken(interval = "15 min", by = "Timestamp", colname = "dtime_15") %>%
  
  # Aggregate to one value per 15-min window (mean handles duplicates too)
  group_by(dtime_15, Site_ID) %>%
  summarise(
    Logger_TempC = mean(Logger_TempC, na.rm = T),
    DO_conc_mgL = mean(DO_conc_mgL, na.rm = T), 
    DO_perc = mean(DO_perc, na.rm = T),
    Q = mean(Q, na.rm = T),
    .groups = "drop") %>%
  
  rename(Timestamp = dtime_15)


wl <- read_csv("../Sensors/Hydrology/data/WL_JL_250403.csv") %>%
  rename(Timestamp = Timestamp_corrected) %>%
  mutate(Site_ID = substring(Site_Name, 1, 2)) %>%
  select(Site_ID, Timestamp, waterLevel)

baro <- read_csv("data/JL_barometric pressure.csv") %>%
  mutate(datetime = mdy_hm(datetime, tz = "UTC")) %>%
  rename(Timestamp = datetime)

# merge datasets
setDT(do); setDT(wl)
setkey(wl, Site_ID, Timestamp)
setkey(do, Site_ID, Timestamp)
DelMET <- wl[do, roll = 7200]

setDT(baro)
setkey(baro, Timestamp)
setkey(DelMET, Timestamp)
DelMET <- baro[DelMET, roll = 7200]

# fix column names to match functions
DelMET <- DelMET %>%
  rename(
    dtime = Timestamp,
    oxy   = DO_conc_mgL,
    temp  = Logger_TempC
  )

DelMET$Date <- DelMET$dtime

# convert barometric pressure from kPa to mmHg
# 1 kPa = 7.50062 mm Hg
DelMET$bp <- DelMET$atm_pressure_kpa * 7.50062
cat("BP range after conversion (should be ~700-780 mm Hg):",
    range(DelMET$bp, na.rm = TRUE), "\n")

# set fixed parameters
ts <- 1/96  # timestep as fraction of day (1/96 = 15 min)
nbatch <- 56000
ncut <- 5000 # what is an appropriate # of iterations to cut? (~20% of nbatch?)
initial <- c(1, -1, 0) # initial values for GPP, ER, log(sigma)

# make sure timesteps are really 15 minutes throughout the dataset
# this is VERY important for the model

DelMET %>%
  arrange(Site_ID, dtime) %>%
  group_by(Site_ID) %>%
  mutate(dt_min = as.numeric(difftime(dtime, lag(dtime), units = "mins"))) %>%
  summarise(
    min_dt = min(dt_min, na.rm = TRUE),
    median_dt = median(dt_min, na.rm = TRUE), # should be 15
    max_dt = max(dt_min, na.rm = TRUE),
    n_not_15 = sum(dt_min != 15, na.rm = TRUE) # it's fine if there are some gaps
  )

# site-specific parameters
# k600 values are from flux measurements on 9/30/2023 (m/hour)
ts_k = 0.002285866294
dk_k = 0.01296964882
nd_k = 0.02
# nd_k = 0.0272 # from Rabaey et al 2024 for small ponds

site_params <- data.frame(
  Site_ID = c("TS", "DK", "ND"),
  lat     = c(39.0, 39.0, 39.0), # could also specify a bit better
  longobs = c(75.4, 75.4, 75.4), # could also specify a bit better
  k600    = c(ts_k, dk_k, nd_k), # m/h
  scale = c(0.07, 0.08, 0.12)
)

# filter dates you want to run
# O2-CO2 departure data time limits: 4/14/2021 -- 5/22/2022

subset <- DelMET %>%
  filter(dtime > as.POSIXct("2021-11-01 00:00:00", tz = "UTC") &
           dtime < as.POSIXct("2021-11-03 00:02:00", tz = "UTC"))

# create folder for all the plots
dir.create("output/metabolism plots", recursive = TRUE, showWarnings = FALSE)

# run loop
for(site in unique(subset$Site_ID)) {
  
  site_data <- subset %>% filter(Site_ID == site)
  
  lat       <- site_params$lat[site_params$Site_ID == site]
  longobs   <- site_params$longobs[site_params$Site_ID == site]
  k600_site <- site_params$k600[site_params$Site_ID == site]
  scale <- site_params$scale[site_params$Site_ID == site]
  k600      <- k600_site  # mout() references k600 by name
  Wetland   <- site       # mout() references Wetland by name
  
  dates <- as.character(unique(as.Date(site_data$dtime)))
  
  for(day_char in dates) {
    
    day <- as.Date(day_char)
    
    # Assign longstd based on EST vs EDT
    # EDT (summer, UTC-4): standard meridian = 60
    # EST (winter, UTC-5): standard meridian = 75
    longstd <- ifelse(day >= as.Date("2021-03-14") & day < as.Date("2021-11-07"), 60, 75)
    
    # Subset 00:00 day 1 to 02:00 day 2 (26 hours)
    start_time <- as.POSIXct(day, tz = "UTC")
    end_time   <- start_time + 26 * 3600
    day_data   <- site_data %>% filter(dtime >= start_time & dtime <= end_time)
    
    day_label <- format(day, "%Y-%m-%d")
    
    # Calculate light with correct longstd for this day
    day_data$light <- lightest(time    = day_data$dtime,
                               lat     = lat,
                               longobs = longobs,
                               longstd = longstd)
    
    # Get zmix from daily mean water level 
    zmix <- mean(day_data$waterLevel, na.rm = TRUE)
    
    if(is.na(zmix) | zmix <= 0) {
      cat("SKIPPING", site, day_label, "- zmix is NA or <= 0 (waterLevel missing?)\n")
      next
    }
    if(zmix < 0.05) {
      cat("SKIPPING", site, day_label, "- zmix =", round(zmix, 3), "m, too shallow for stable model\n")
      next
    }
    
    zmix_sd <- sd(day_data$waterLevel, na.rm = TRUE)
    if(!is.na(zmix_sd) && zmix_sd / zmix > 0.2) {
      cat("WARNING:", site, day_label, "- water level CV =",
          round(zmix_sd / zmix, 2), "- zmix unstable within day\n")
    }
    
    # Get daily mean barometric pressure
    bp <- mean(day_data$bp, na.rm = TRUE)
    
    if(is.na(bp) | bp <= 0) {
      cat("SKIPPING", site, day_label, "- bp is NA or <= 0 (baro data missing?)\n")
      next
    }
    if(bp < 700 | bp > 800) {
      cat("WARNING:", site, day_label, "- bp =", round(bp, 1),
          "mm Hg after kPa conversion - check baro sensor data for this day\n")
    }
    
    # Diagnostics: skip bad days
    if(nrow(day_data) == 0) {
      cat("SKIPPING", site, day_label, "- no data\n")
      next
    }
    
    na_check <- sapply(day_data[, c("oxy", "temp", "light")], function(x) sum(is.na(x)))
    if(any(na_check > 0)) {
      cat("SKIPPING", site, day_label, "- NAs found:",
          paste(names(na_check), na_check, sep = "=", collapse = ", "), "\n")
      next
    }
    
    if(sum(day_data$light) == 0) {
      cat("SKIPPING", site, day_label, "- light sums to zero\n")
      next
    }
    
    if(any(day_data$oxy <= 0)) {
      cat("SKIPPING", site, day_label, "- oxy has zero or negative values\n")
      next
    }
    
    if(any(day_data$temp < 0 | day_data$temp > 40)) {
      cat("WARNING:", site, day_label, "- temp outside 0-40C, check data\n")
    }
    
    test_val <- tryCatch(
      lakeMET(initial, day_data$oxy, zmix, day_data$temp,
              day_data$light, bp, ts, k600_site),
      error = function(e) NA
    )
    if(!is.finite(test_val)) {
      cat("SKIPPING", site, day_label, "- lakeMET returned", test_val, "at initial params\n")
      cat("  oxy range  :", range(day_data$oxy),   "\n")
      cat("  temp range :", range(day_data$temp),  "\n")
      cat("  light range:", range(day_data$light), "\n")
      cat("  bp:", round(bp, 1), "| zmix:", round(zmix, 3),
          "| k600:", k600_site, "| ts:", ts, "\n")
      next
    }
    
    # Run MCMC metabolism
    met.out <- metrop(lakeMET,
                      initial = initial,
                      nbatch  = nbatch,
                      scale   = scale,
                      oxy     = day_data$oxy,
                      zmix    = zmix,
                      temp    = day_data$temp,
                      light   = day_data$light,
                      ts      = ts,
                      bp      = bp,
                      k600    = k600_site)
    
    # Adapt scale for next day
    accept <- met.out$accept
    if(accept < 0.15)      { scale <- scale * 0.5  
    } else if(accept < 0.20) { scale <- scale * 0.8  
    } else if(accept > 0.60) { scale <- scale * 2.0  
    } else if(accept > 0.45) { scale <- scale * 1.2  
    }
    scale <- pmax(0.01, pmin(scale, 2.0))
    
    # Save output
    m.out <- mout(day_data, met.out)
    m.out_df <- as.data.frame(t(m.out), stringsAsFactors = FALSE)
    colnames(m.out_df) <- names(Excel)
    Excel <- rbind(Excel, m.out_df)
    
    # Save O2 fit plot
    pdf(file = paste0("output/metabolism plots/", site, "_", day_label, ".O2.pdf"))
    lakeMETplot(
      data = day_data,
      GPP  = quantile(met.out$batch[(ncut:nbatch), 1], 0.5),
      ER   = quantile(met.out$batch[(ncut:nbatch), 2], 0.5),
      bp   = bp, zmix = zmix, k600 = k600_site, ts = ts
    )
    dev.off()
    
    # Save MCMC diagnostic plots
    
    # Previous version of plots without burn-in period
    
    # pdf(file = paste0("output/metabolism plots/", site, "_", day_label, ".diagnostics.pdf"))
    # par(mfrow = c(3, 2))
    # 
    # # GPP
    # plot(met.out$batch[ncut:nbatch, 1], type = "l",
    #      main = paste(site, day_label, "- GPP trace"),
    #      xlab = "Iteration", ylab = "GPP (g O2 m-2 d-1)")
    # plot(density(met.out$batch[ncut:nbatch, 1]),
    #      main = "GPP posterior density",
    #      xlab = "GPP (g O2 m-2 d-1)")
    # abline(v = quantile(met.out$batch[ncut:nbatch, 1], c(0.025, 0.5, 0.975)),
    #        col = c("red", "blue", "red"), lty = c(2, 1, 2))
    # 
    # # ER
    # plot(met.out$batch[ncut:nbatch, 2], type = "l",
    #      main = paste(site, day_label, "- ER trace"),
    #      xlab = "Iteration", ylab = "ER (g O2 m-2 d-1)")
    # plot(density(met.out$batch[ncut:nbatch, 2]),
    #      main = "ER posterior density",
    #      xlab = "ER (g O2 m-2 d-1)")
    # abline(v = quantile(met.out$batch[ncut:nbatch, 2], c(0.025, 0.5, 0.975)),
    #        col = c("red", "blue", "red"), lty = c(2, 1, 2))
    # 
    # # Sigma
    # plot(exp(met.out$batch[ncut:nbatch, 3]), type = "l",
    #      main = paste(site, day_label, "- Sigma trace"),
    #      xlab = "Iteration", ylab = "Sigma")
    # plot(density(exp(met.out$batch[ncut:nbatch, 3])),
    #      main = "Sigma posterior density",
    #      xlab = "Sigma")
    # abline(v = quantile(exp(met.out$batch[ncut:nbatch, 3]), c(0.025, 0.5, 0.975)),
    #        col = c("red", "blue", "red"), lty = c(2, 1, 2))
    # 
    # # Acceptance rate and parameter values in footer
    # mtext(paste("Acceptance rate:", round(met.out$accept, 3),
    #             "| zmix:", round(zmix, 3), "m",
    #             "| bp:", round(bp, 1), "mm Hg",
    #             "| nbatch:", nbatch, "| ncut:", ncut),
    #       side = 1, line = -1.5, outer = TRUE, cex = 0.8)
    # 
    # par(mfrow = c(1, 1))
    # dev.off()
    
    
    # Save all diagnostic plots (all iterations including burn-in)
    
    pdf(file = paste0("output/metabolism plots/", site, "_", day_label, ".diagnostics.pdf"))
    par(mfrow = c(2, 1))
    
    # Full chain with burn-in shown
    plot(ts(met.out$batch),
         main = paste(site, day_label, "- full chain (all iterations)"))
    
    # Add vertical line showing where burn-in cutoff is
    abline(v = ncut, col = "red", lty = 2, lwd = 2)
    
    # Post burn-in only
    plot(ts(met.out$batch[ncut:nbatch, ]),
         main = paste(site, day_label, "- post burn-in only (ncut =", ncut, ")"))
    
    mtext(paste("Acceptance rate:", round(met.out$accept, 3),
                "| nbatch:", nbatch, "| ncut:", ncut),
          side = 1, line = -1.5, outer = TRUE, cex = 0.8)
    
    par(mfrow = c(1, 1))
    dev.off()
    
    cat("Done:", site, day_label,
        "| accept:", round(met.out$accept, 3),
        "| zmix:", round(zmix, 3), "m",
        "| bp:", round(bp, 1), "mm Hg\n")
  }
}

# Save results
write_csv(Excel, "metabolism_all_sites.csv")
cat("Saved", nrow(Excel), "rows to metabolism_all_sites.csv\n")
