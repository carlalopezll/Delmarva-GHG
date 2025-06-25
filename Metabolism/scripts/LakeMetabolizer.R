#### Experimenting with LakeMetabolizer ####

install.packages("LakeMetabolizer")
library(LakeMetabolizer)
library(readxl)
library(stringr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(scales)

######## ~~~~~~ DO Data Set Up ~~~~~~~ ########

setwd("~/Hotchkiss Lab/Delmarva Wetlands Data/Personal Research Data/Metabolism/LakeMetabolizer")

ND_DO_2023_2024 <- read_excel("~/Hotchkiss Lab/Delmarva Wetlands Data/Personal Research Data/DO Sensors Analysis/Excel Files/ND_DO_2023_2024.xlsx")

ND_DO_2023_2024[c('Date', 'Time')] <- str_split_fixed(ND_DO_2023_2024$Eastern_Standard_Time, ' ', 2)
ND_DO_2023_2024$Date <- as.Date(ND_DO_2023_2024$Date)

ND_DO_2023_2024$Timestamp_corrected<-as_datetime(ND_DO_2023_2024$Eastern_Standard_Time)
str(ND_DO_2023_2024$Timestamp_corrected)

names(ND_DO_2023_2024)[6] <- "DO_mgL"
names(ND_DO_2023_2024)[7] <- "DO_Saturation"


## Separate into singular days when we did incubations ##

ND_DO_July <- ND_DO_2023_2024[ND_DO_2023_2024$Date > "2023-07-17" &
                                ND_DO_2023_2024$Date < "2023-07-19",]

ND_DO_September <- ND_DO_2023_2024[ND_DO_2023_2024$Date > "2023-09-28" &
                                     ND_DO_2023_2024$Date < "2023-09-30",]

ND_DO_November3 <- ND_DO_2023_2024[ND_DO_2023_2024$Date > "2023-11-02" &
                                     ND_DO_2023_2024$Date < "2023-11-04",]

ND_DO_February <- ND_DO_2023_2024[ND_DO_2023_2024$Date > "2024-02-24" &
                                     ND_DO_2023_2024$Date < "2024-11-26",]


## Establish parameters for the singular days. Temperature is in Celsius while DO is in mg/L ##

JulyTemp_Avg = mean(ND_DO_July$Temp_C)
JulyDO_Avg = mean(ND_DO_July$DO_mgL)

SeptemberTemp_Avg = mean(ND_DO_September$Temp_C)
SeptemberDO_Avg = mean(ND_DO_September$DO_mgL)

NovemberTemp_Avg = mean(ND_DO_November3$Temp_C)
NovemberDO_Avg = mean(ND_DO_November3$DO_mgL)

FebruaryTemp_Avg = mean(ND_DO_February$Temp_C)
FebruaryDO_Avg = mean(ND_DO_February$DO_mgL)


## Find the average wind speed that day for the location in question by using a nearby weather tower. We got ours using the Weather Underground service tower. Divide by 2 to account for the blocked wind from tree cover, convert from mph to m/s through multiplication by 0.447 ##

July_AvgWind = (4.83/2)
JulyU10 = July_AvgWind*0.447

September_AvgWind = (6.67/2)
SeptemberU10 = September_AvgWind*0.447

November_AvgWind = (2.5/2)
NovemberU10 = November_AvgWind*0.447

February_AvgWind = (5.6/2)
FebruaryU10 = February_AvgWind*0.447

## Surface Area of ND in km^2 given by data from Carla's first chapter ##

LA = 1.364


######## ~~~~~~ Finding k values ~~~~~~~ ########

## Plug into formula found from by Vachon et al. ##

Julyk600.cmh = (2.51+1.48*JulyU10+0.39*JulyU10*log10(LA))
Septemberk600.cmh = (2.51+1.48*SeptemberU10+0.39*SeptemberU10*log10(LA))
Novemberk600.cmh = (2.51+1.48*NovemberU10+0.39*NovemberU10*log10(LA))
Februaryk600.cmh = (2.51+1.48*FebruaryU10+0.39*FebruaryU10*log10(LA))

## convert from cm/h to m/d by dividing by 4.167 ##

Julyk600.md = (2.51+1.48*JulyU10+0.39*JulyU10*log10(LA))/4.167
Septemberk600.md = (2.51+1.48*SeptemberU10+0.39*SeptemberU10*log10(LA))/4.167
Novemberk600.md = (2.51+1.48*NovemberU10+0.39*NovemberU10*log10(LA))/4.167
Februaryk600.md = (2.51+1.48*FebruaryU10+0.39*FebruaryU10*log10(LA))/4.167


## Get the kO2 value. This gives us a prediction of where our k value should be for the rate of oxygen leaving the wetland system, given the wind speed estimates ##

JulykO2.cmh <- k600.2.kGAS.base(k600=Julyk600.cmh, temperature= JulyTemp_Avg, gas='O2')
SeptemberkO2.cmh <- k600.2.kGAS.base(k600=Septemberk600.cmh, temperature= SeptemberTemp_Avg, gas='O2')
NovemberkO2.cmh <- k600.2.kGAS.base(k600=Novemberk600.cmh, temperature= NovemberTemp_Avg, gas='O2')
FebruarykO2.cmh <- k600.2.kGAS.base(k600=Februaryk600.cmh, temperature= FebruaryTemp_Avg, gas='O2')


JulykO2.md <- k600.2.kGAS.base(k600=Julyk600.md, temperature= JulyTemp_Avg, gas='O2')
SeptemberkO2.md <- k600.2.kGAS.base(k600=Septemberk600.md, temperature= SeptemberTemp_Avg, gas='O2')
NovemberkO2.md <- k600.2.kGAS.base(k600=Novemberk600.md, temperature= NovemberTemp_Avg, gas='O2')
FebruarykO2.md <- k600.2.kGAS.base(k600=Februaryk600.md, temperature= FebruaryTemp_Avg, gas='O2')

## This seems like an overestimate, since most literature says that there is negligible mixing from wind due to the size and location of the wetlands/ponds. Most literatures suggest that these wetlands will likely fall between 0.30 to 0.90 m/d



######## ~~~~~~ Checking Metabolism Estimates ~~~~~~~ ########

#### we found k through gas flux equation, so we use kCO2 as our k value ####

## Converting k600 to kCO2 using LakeMetabolizer, assuming k600 is correct from initial equation in O2 ##

JulykCO2_Metabolizer.md <- k600.2.kGAS.base(k600=Julyk600.md, temperature= JulyTemp_Avg, gas='CO2')

## Converting k600 to kCO2 using Matthews et al., assuming k600 is correct from initial equation in O2 ##

Schmidt_CO2 <- 1911.1 - (118.11*JulyTemp_Avg)+(3.4527*(JulyTemp_Avg^2))-(0.041320*(JulyTemp_Avg^3))
JulykCO2_Matthews.cmh <- Julyk600.cmh*((600^0.67)/(Schmidt_CO2^0.67))
JulykCO2_Matthews.md <- JulykCO2_Matthews.cmh/4.167

## Comparing Metabolizer to Matthews, there is about 0.14 difference. While it makes a little bit of a difference, I don't believe it is significant. We will be using Matthews for our research, but it is good to see how LakeMetabolizer compares with similar data.
