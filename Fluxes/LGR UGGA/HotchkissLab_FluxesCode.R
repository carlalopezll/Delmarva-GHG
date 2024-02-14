#################################
#EXAMPLE FLUX CALC FROM CHAMBERS
#Calculations to get from ppm s-1 to mmol m-2 d-1
#Last updated:07/27/2020 ARJ
### 
#################################
######
Fluxes <- read.csv("July2020MasterFlux.csv")
View(Fluxes)
FluxesQC<-subset(Fluxes,Use=="Y")
FluxesQC
## Delete notes column ##
FluxesQC$Notes <- NULL
view(FluxesQC)

## [1] List of parameters and data inputs ##

# SLOPE = LGR flux derived from linear model slope (ppm s-1)
# R = Universal gas constant (m3 Pa mol-1 K-1)
R <- 8.31451
# temp.C = average gas temperature (C) during chamber incubation
# Patm.kPa = Atmospheric pressure (kPa) during chamber incubation 
Patm.kPa <- 98
# chamber.mm = Chamber Volume (mm3) / Surface Area (mm2) = (mm)
chamber.mm <- 55.6
#### input data file
#### Before step 3, need to have 1 slope vlaue for each rep/chamber & 2 mean(data$temp) for each chamber

temp.C <- FluxesQC$TempC
SLOPECO2 <- FluxesQC$SlopeCO2.ppm.s
SLOPECH4 <- FluxesQC$SlopeCH4.ppm.s
#################################

## [2] Conversion functions ##

# convert 1/seconds to 1/day
# (1/s)*(60s/min)*(60min/hr)*(24hr/d)
sec.day <- function(sec){
  day <- sec*60*60*24 
  day
}
###### 

#################################

## [3a] Flux conversion equation for Carbon Dioxide ##

flux <- function(SlopeCO2.ppm.s, chamber.mm, R, Patm.kPa, temp.C){
  
  # convert flux term from ppm/s to 1/d
  # NOTE: mathematically, ppm = 1e-06 or 10^-6
  flux.d <- sec.day(SlopeCO2.ppm.s)*(1e-06) 
  
  # convert chamber V/SA ratio from mm to m (1 m = 1000 mm)
  chamber.m <- chamber.mm / 1000 
  
  # convert temp.C to temp.K
  temp.K <- temp.C + 273.15
  
  # convert Patm.kPa to Pa (1 kPa = 1000 Pa)
  Pa <- Patm.kPa*1000
  
  flux <- ( flux.d /  ( R*temp.K*(1/Pa) ) ) * chamber.m * (1000) # 1000mmol/mol
  flux
}


flux2 <- function(SlopeCH4.ppm.s, chamber.mm, R, Patm.kPa, temp.C){
  
  # convert flux term from ppm/s to 1/d
  # NOTE: mathematically, ppm = 1e-06 or 10^-6
  flux.d <- sec.day(SlopeCH4.ppm.s)*(1e-06) 
  
  # convert chamber V/SA ratio from mm to m (1 m = 1000 mm)
  chamber.m <- chamber.mm / 1000 
  
  # convert temp.C to temp.K
  temp.K <- temp.C + 273.15
  
  # convert Patm.kPa to Pa (1 kPa = 1000 Pa)
  Pa <- Patm.kPa*1000
  
  flux2 <- ( flux.d /  ( R*temp.K*(1/Pa) ) ) * chamber.m * (1000) # 1000mmol/mol
  flux2
}

#################################

## [4]Run flux conversion equation ##
# new equation with better tracking of unit conversions and creates new coloumn
FluxesQC$CO2flux.mmnolm2d <-flux(SLOPECO2, chamber.mm, R, Patm.kPa, temp.C)
FluxesQC$CO2flux.mmnolm2d
FluxesQC$CH4flux.mmnolm2d <-flux2(SLOPECH4, chamber.mm, R, Patm.kPa, temp.C)
FluxesQC$CH4flux.mmnolm2d


##############################
#######Done organizing data###
##############################

##### Create GRAPHS#####
## Organize by Region ( Coweeta or Poverty Creek)##

## Poverty Creek##
Pvty <- FluxesQC[FluxesQC$WetlandID=="PVTY",]
## Coweeta##
CWT <- FluxesQC[FluxesQC$WetlandID=="CWT",]

#### Organinze by site( Ball Creek, Old 5, Fern Gully or Jacobs Ladder)
##Ball Creek - located in Coweeta##
BC <- FluxesQC[FluxesQC$Site=="BC",]
## Old 5 - located in Coweeta##
O5 <- FluxesQC[FluxesQC$Site=="O5",]
## Fern Gully - located in Poverty Creek##
FG <- FluxesQC[FluxesQC$Site=="FG",]
## Jacobs Ladder - located in Poverty Creek##
JL <-FluxesQC[FluxesQC$Site=="JL",]

## change name of "WetlandID" column to "Region"
FluxesQC$Region <- FluxesQC$WetlandID


#stats
## STATS
summary(FluxesQC)
summary(CWT)
summary(Pvty)
summary(BC)
summary(O5)
summary(JL)
summary(FG)


## Load All Packages##
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"plotly" %in% installed.packages()) install.packages("plotly")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"hrbrthemes" %in% installed.packages()) install.packages("hrbrthemes")
if (!"viridis" %in% installed.packages()) install.packages("viridis")
if (!"ggthemes" %in% installed.packages()) install.packages("ggthemes")
if (!"grid" %in% installed.packages()) install.packages("grid")
if (!"gridExtra" %in% installed.packages()) install.packages("gridExtra")
if (!"scales" %in% installed.packages()) install.packages("scales")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")

# Open Libaries##

library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(scales)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

#install and run color brewer
library(RColorBrewer)
display.brewer.all()


# Tiddying data frame. dealing with factor##
##https://swcarpentry.github.io/r-novice-inflammation/12-supp-factors/#\BC.23QC = BC.23[-c(5460,5461, 5462,5463),]

## GRAPHS###
############################################
### Carbon dioxide for all sites###
p <- ggplot(data = FluxesQC, aes(Site, CO2flux.mmnolm2d)) +
  theme_classic()+
  scale_fill_brewer(palette="BuPu")+
  geom_boxplot()+
  aes(color = Region)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_x_discrete(name = "Site") +
  ylab(bquote('CO2 ('*mmol ~ m^-2~d^-1*')'))

p


############################################

### Methane for all Sites ###


p <- ggplot(data = FluxesQC, aes(Site, CH4flux.mmnolm2d)) +
  theme_classic()+
  scale_fill_brewer(palette="BuPu")+
  geom_boxplot()+
  aes(color = Region)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_discrete(name = "Site") +
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

p

### Zoomed in Methane for all sites###

p <- ggplot(data = FluxesQC, aes(Site, CH4flux.mmnolm2d)) +
  theme_classic()+
  scale_fill_brewer(palette="BuPu")+
  geom_boxplot()+
  aes(color = Region)+
  scale_y_continuous(limits = c(0, 100),breaks = scales::pretty_breaks(n = 10))+
  scale_x_discrete(name = "Site") +
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

p
################################################
## Dates and  Methane##
p <- ggplot(data = FluxesQC, aes(Date, CH4flux.mmnolm2d)) +
  theme_classic()+
  scale_fill_brewer(palette="BuPu")+
  geom_boxplot()+
  aes(color = Region)+
  ylim(0,100)+
  scale_x_discrete(name = "Site") +
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

p

###############################################
### Dates ND Carbon Dioxide ###

p <- ggplot(data = FluxesQC, aes(Date, CO2flux.mmnolm2d)) +
  theme_classic()+
  scale_fill_brewer(palette="BuPu")+
  geom_boxplot()+
  aes(color = Region)+
  ylim(0,100)+
  scale_x_discrete(name = "Site") +
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

p

###############################################
#### 


### Create CSV File
write.csv(FluxesQC, "C:\\Users\\Owner\\Documents\\FluxesUpdated.csv")

# did it work??

#################################
