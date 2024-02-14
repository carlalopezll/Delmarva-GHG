###
Fluxes <- read.csv("02Testing11.19.20.csv")
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

library(forcats)
head(FluxesQC)




############################################

### Carbon dioxide for all sites### (Colored by Region, Seperated by Site)
ggplot(FluxesQC, aes(x=Site, y=CO2flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  scale_x_discrete(limits=c("FG", "JL", "O5","BC"))+
  geom_jitter(color="purple", size=0.9, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylab(bquote('CO2 ('*mmol ~ m^-2~d^-1*')'))


## Carbon dioxide for all sites### (Colored by Site, Seperated by Region)
ggplot(FluxesQC, aes(x=Region, y=CO2flux.mmnolm2d, fill=Site))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylab(bquote('CO2 ('*mmol ~ m^-2~d^-1*')'))

############################################
### Methane for all Sites ### Colored by Region, Seperated by Site)

ggplot(FluxesQC, aes(x=Site, y=CH4flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="purple", size=0.9, alpha=0.9) +
  scale_x_discrete(limits=c("FG", "JL", "O5","BC"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))


### Zoomed in Methane for all sites###

ggplot(FluxesQC, aes(x=Site, y=CH4flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="purple", size=0.9, alpha=0.9) +
  scale_x_discrete(limits=c("FG", "JL", "O5","BC"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))+
  ylim(0,100)+
  scale_y_continuous(limits = c(0, 100),breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

################################################
## Dates and  Methane##
ggplot(FluxesQC, aes(x=Date, y=CH4flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))+
  ylim(0,100)+
  scale_y_continuous(limits = c(0, 100),breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

# same as above but zoomed in
ggplot(FluxesQC, aes(x=Date, y=CH4flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))+
  scale_y_continuous(limits = c(0, 50),breaks = scales::pretty_breaks(n = 10))+
  ylab(bquote('CH4 ('*mmol ~ m^-2~d^-1*')'))

###############################################
### Dates ND Carbon Dioxide ###

ggplot(FluxesQC, aes(x=Date, y=CO2flux.mmnolm2d, fill=Site))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylab(bquote('CO2 ('*mmol ~ m^-2~d^-1*')'))

ggplot(FluxesQC, aes(x=Date, y=CO2flux.mmnolm2d, fill=Region))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylab(expression(paste("C", H[4], " (", mu,"mmol ", L^-1,")"))) 


ggplot(FluxesQC, aes(x=Date, y=CO2flux.mmnolm2d, fill=Wet.Dry))  +
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette="Blues")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylab(bquote('CO2 ('*mmol ~ m^-2~d^-1*')'))


####Testing of INDUATION AND FLUXES####
#BC##
ggplot(FluxesQC, aes(x=RC.Medians, y=CO2flux.mmnolm2d, color=Date, shape = Site)) + 
  geom_point()

ggplot(FluxesQC, aes(x=RC.Medians, y=CH4flux.mmnolm2d,color=Date, shape = Site)) +
  ylim(0,100)+
  geom_point()

ggplot(FluxesQC, aes(x=Date, y=RC.Medians, group=Site, color=Site)) + 
  geom_line()

##
##O5#

# FG#

#JL#
ggplot(FluxesQC, aes(x=Date, y=RC.Medians, color=Date)) + 
  geom_point()