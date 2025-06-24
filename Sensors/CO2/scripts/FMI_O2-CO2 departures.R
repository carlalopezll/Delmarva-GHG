#Paired O2 - CO2 departures analysis, following the method of Vachon et al. 2020
#Frances Iannucci
#November 2023
#Updated 4/2024 to change where eddy data is stored.

#REMEMBER WHAT TIME ZONE YOU'RE IN

#setwd('~/MacroCO2/CO2 Sensor Data')
require(dplyr)
require(ggplot2)
require(lubridate)
require(streamMetabolizer)
require(neonUtilities)
require(zoo)
require(lmodel2)
require(cowplot)

#### EXTRACT ATMOSPHERIC CO2 CONCENTRATIONS FROM EDDY COVARIANCE BUNDLE ####
#just run this once, then load the saved products later.

# install.packages('BiocManager')
# BiocManager::install('rhdf5')
require(rhdf5)

#need to set specific start/end months for each site because of how much data is being downloaded.
siteName='NIWO'
startDate="2021-12"
endDate="2023-12"
zipsByProduct(dpID="DP4.00200.001", site=siteName, startdate=startDate, 
              enddate=endDate, check.size = F, include.provisional = T, 
              savepath = '~/Eddy Covariance Data/') #saves to computer, not drive

#extract isoCo2 data for all sites
eddy= stackEddy(filepath='~/Eddy Covariance Data/filesToStack00200/', 
                level="dp01", var="rtioMoleDryCo2", avg=6)
list2env(eddy, .GlobalEnv)

#pull out relevant site-specific data. update site and time zone.
siteName='BONA'
timeZone= ifelse(siteName=='BONA', 'America/Anchorage',
                 ifelse(siteName=='NIWO', 'America/Denver',
                        ifelse(siteName=='KONA', 'America/Chicago',
                               ifelse(siteName=='WREF', 'America/Vancouver',
                                      ifelse(siteName=='ORNL', 'America/New_York', print('You have a typo! Fix it!'))))))
atm= BONA[,c(2,4,5,10)]

atm= filter(atm, verticalPosition=='010')
atm$data.isoCo2.rtioMoleDryCo2.mean= ifelse(atm$qfqm.isoCo2.rtioMoleDryCo2.qfFinl!=0, NA, atm$data.isoCo2.rtioMoleDryCo2.mean)
atm$timeEnd= round_date(atm$timeEnd, unit = '15 minute')
atm$timeEnd= with_tz(atm$timeEnd, tzone = timeZone)
names(atm)[c(2,3)]= c('datetime','co2.atm.ppmv')
atm= atm[,2:3]
save(atm, file = paste0(getwd(),'/NEON Sensor Data/',siteName,'.atm.isoCO2.RData'))

#### DOWNLOAD AND SAVE O2 AND BAROMETRIC PRESSURE ####
siteName="CARI"
startDate="2021-08"
endDate="2023-12"
timeZone= ifelse(siteName=='CARI', 'America/Anchorage',
                 ifelse(siteName=='COMO', 'America/Denver',
                        ifelse(siteName=='KING', 'America/Chicago',
                               ifelse(siteName=='MART', 'America/Vancouver',
                                      ifelse(siteName=='WALK', 'America/New_York', print('You have a typo! Fix it!'))))))

bp=loadByProduct(dpID="DP1.00004.001", site=siteName, startdate=startDate, 
                  enddate=endDate, package="basic", check.size = F, include.provisional = T)
list2env(bp, .GlobalEnv)
BP_1min$startDateTime<-as.POSIXct(BP_1min$startDateTime,format="%Y-%m-%dT%H:%M", tz="UTC")
bp=BP_1min[,c("startDateTime","staPresMean","staPresFinalQF")]
bp$staPresMean= ifelse(bp$staPresFinalQF!=0, NA, bp$staPresMean)
bp$startDateTime=round_date(bp$startDateTime,unit="15 minute")
bp=summarize(group_by(bp, startDateTime), barPres=mean(staPresMean, na.rm=T))  #Calculates 15 minute averages
bp$barPres=na.spline(bp$barPres,maxgap=24)  #Fills data gaps using spline curve, max of 6 hours
bp$startDateTime=with_tz(bp$startDateTime, tzone = timeZone)
names(bp)[1]='datetime'
save(bp, file = paste0(getwd(),'/NEON Sensor Data/',siteName,'.bp.RData'))

waq=loadByProduct(dpID="DP1.20288.001", site=siteName, startdate=startDate,
                   enddate=endDate, package="basic", check.size = F, include.provisional = T)
list2env(waq, .GlobalEnv)
waq_instantaneous$startDateTime=as.POSIXct(waq_instantaneous$startDateTime,format="%Y-%m-%dT%H:%M", tz="UTC")
waqS2=waq_instantaneous[(waq_instantaneous$horizontalPosition=='102')|(waq_instantaneous$horizontalPosition=="112"),]
waqS2=waqS2[,c("startDateTime","dissolvedOxygen","dissolvedOxygenFinalQF")]
waqS2$dissolvedOxygen= ifelse(waqS2$dissolvedOxygenFinalQF!=0, NA, waqS2$dissolvedOxygen)
waqS2$startDateTime=round_date(waqS2$startDateTime,unit="15 minute")
waqS2=summarize(group_by(waqS2, startDateTime), dissolvedOxygen=mean(dissolvedOxygen, na.rm=T))
waqS2$dissolvedOxygen=na.spline(waqS2$dissolvedOxygen,maxgap=24) #max gap of 6 hours
waqS2$startDateTime=with_tz(waqS2$startDateTime, tzone = timeZone)
names(waqS2)[1]='datetime'
save(waqS2, file = paste0(getwd(),'/NEON Sensor Data/',siteName,'.DO.WITH.FLAGGED.DATA.RData'))

#### LOAD/MERGE DATA AND CALCULATE DEPARTURES ####
#O2, BP, and atm CO2 prepped above.
#CO2 and temperature prepped in CO2SensorCorrections script.

#COMO
#load O2 and BP data
load(paste0(getwd(),'/NEON Sensor Data/COMO.DO.RData'))
load(paste0(getwd(),'/NEON Sensor Data/COMO.bp.RData'))
mergedData<-merge(waqS2, bp, all.x = T)

#load 15 minute co2 and temperature data
load('COMO.CO2.15min.RData')

mergedData<-merge(mergedData, co2.averaged)

#calculate DO.sat:
mergedData$barPres= mergedData$barPres*10
mergedData$DO.sat= calc_DO_sat(mergedData$NEON.temp, mergedData$barPres)
mergedData$barPres= mergedData$barPres/10 #convert back for co2 sat calculations

#load atmospheric CO2 data
load(paste0(getwd(),'/NEON Sensor Data/NIWO.atm.isoCO2.RData'))
atm$co2.atm.ppmv= ifelse(atm$co2.atm.ppmv>450, NA, atm$co2.atm.ppmv)

mergedData= merge(mergedData, atm, all.x = T)
mergedData$co2.atm.ppmv= na.spline(mergedData$co2.atm.ppmv, maxgap = 24)

mergedData$site= 'COMO'

allData= mergedData

#WALK
#load O2 and BP data
load(paste0(getwd(),'/NEON Sensor Data/WALK.DO.RData'))
load(paste0(getwd(),'/NEON Sensor Data/WALK.bp.RData'))
mergedData= merge(waqS2, bp, all.x = T)

#filter out weird O2 data
mergedData= filter(mergedData, datetime<'2022-01-03 0:00'|datetime>='2022-01-04 13:00')
mergedData= filter(mergedData, datetime<'2023-10-02 10:30'|datetime>='2023-10-02 12:00')
mergedData= filter(mergedData, datetime!='2023-10-05 23:15')

#load 15 minute co2 and temperature data
load('WALK.CO2.15min.RData')

mergedData= merge(mergedData, co2.averaged)

#calculate DO.sat:
mergedData$barPres= mergedData$barPres*10
mergedData$DO.sat= calc_DO_sat(mergedData$NEON.temp, mergedData$barPres)
mergedData$barPres= mergedData$barPres/10 #convert back for co2 sat calculations

#load atmospheric CO2 data
load(paste0(getwd(),'/NEON Sensor Data/ORNL.atm.isoCO2.RData'))

mergedData= merge(mergedData, atm, all.x = T)
mergedData$co2.atm.ppmv= na.spline(mergedData$co2.atm.ppmv, maxgap = 24)

mergedData$site= 'WALK'

allData= rbind(allData, mergedData)

#CARI
#load O2 and BP data
load(paste0(getwd(),'/NEON Sensor Data/CARI.DO.WITH.FLAGGED.DATA.RData'))
load(paste0(getwd(),'/NEON Sensor Data/CARI.bp.RData'))
mergedData= merge(waqS2, bp, all.x = T)

require(plotly)
plot_ly(data = mergedData,
        x=~datetime, y=~dissolvedOxygen, type = 'scatter', mode = 'markers')

#O2 data cleaning (add 4 hrs if on east coast)
mergedData= filter(mergedData, datetime<'2021-09-23 7:30' | datetime>'2021-09-23 17:00')
mergedData= filter(mergedData, datetime<'2021-09-28 6:15' | datetime>'2022-01-01')
mergedData= filter(mergedData, datetime<'2022-10-09 4:15' | datetime>'2023-05-18')
#mergedData= filter(mergedData, datetime<'2023-10-16')
  #removing winter data
#mergedData$dissolvedOxygen= ifelse(mergedData$datetime<'2021-06-15', mergedData$dissolvedOxygen * 97.99949/101.325, mergedData$dissolvedOxygen)
    #no co2 data during this time anyway
mergedData$dissolvedOxygen= ifelse(mergedData$datetime>'2022-05-01' & mergedData$datetime<'2022-07-01', mergedData$dissolvedOxygen * 98.93705/101.325, mergedData$dissolvedOxygen)
mergedData= filter(mergedData, datetime<'2022-07-01' | datetime>='2022-07-06 18:30')
# mergedData$dissolvedOxygen= ifelse(mergedData$datetime>'2023-05-18' & mergedData$datetime<'2023-07-05', mergedData$dissolvedOxygen * 98.34325/101.325, mergedData$dissolvedOxygen)
# mergedData= filter(mergedData, datetime<'2023-07-04 20:15' | datetime>'2023-07-05 20:00')
  #incorrect pressure used during DO calibrations. shifts down spring data to match summer.
  #spring 2023 data are just totally missing now? Bobby might have flagged them...?

#load 15 minute averaged co2 and temperature data
load('CARI.CO2.15min.RData')

mergedData= merge(mergedData, co2.averaged)

#calculate DO.sat:
mergedData$barPres= mergedData$barPres*10
mergedData$DO.sat= calc_DO_sat(mergedData$NEON.temp, mergedData$barPres)
mergedData$barPres= mergedData$barPres/10 #convert back for co2 sat calculations

#load atmospheric CO2 data
load(paste0(getwd(),'/NEON Sensor Data/BONA.atm.isoCO2.RData'))

mergedData= merge(mergedData, atm, all.x = T)
mergedData$co2.atm.ppmv= na.spline(mergedData$co2.atm.ppmv, maxgap = 24)

mergedData$site= 'CARI'

allData= rbind(allData, mergedData)

#MART
#load O2 and BP data
load(paste0(getwd(),'/NEON Sensor Data/MART.DO.RData'))
load(paste0(getwd(),'/NEON Sensor Data/MART.bp.RData'))
mergedData= merge(waqS2, bp, all.x = T)

#O2 data cleaning
#mergedData$DO.obs= ifelse(mergedData$solar.time<'2021-05-18', NA, mergedData$DO.obs)
  #calibration was just off here. exclude?
# mergedData= filter(mergedData, datetime<'2022-06-21 2:00' | datetime>'2022-07-18 15:00')
  #DO sensor drift, most likely.
  #check saturation plots again. DO on its own looks fine? And some of this time period is already removed.

#load 15 minute averaged co2 and temp
load('MART.CO2.15min.RData')

mergedData= merge(mergedData, co2.averaged)

#calculate DO.sat:
mergedData$barPres= mergedData$barPres*10
mergedData$DO.sat= calc_DO_sat(mergedData$NEON.temp, mergedData$barPres)
mergedData$barPres= mergedData$barPres/10 #convert back for co2 sat calculations

#load atmospheric CO2 data
load(paste0(getwd(),'/NEON Sensor Data/WREF.atm.isoCO2.RData'))
atm$co2.atm.ppmv<-ifelse(atm$co2.atm.ppmv>700, NA, atm$co2.atm.ppmv)

mergedData= merge(mergedData, atm, all.x = T)
mergedData$co2.atm.ppmv= na.spline(mergedData$co2.atm.ppmv, maxgap = 24)

mergedData$site= 'MART'

allData= rbind(allData, mergedData)

#KING
#load O2 and BP data
load(paste0(getwd(),'/NEON Sensor Data/KING.DO.RData'))
mergedData= filter(mergedData, datetime<'2022-10-02'|datetime>'2023-05-19')

load(paste0(getwd(),'/NEON Sensor Data/KING.bp.RData'))
mergedData= merge(waqS2, bp, all.x = T)

#load 15 minute averaged co2 and temperature
load('KING.CO2.15min.RData')

mergedData= merge(mergedData, co2.averaged)

#calculate DO.sat:
mergedData$barPres= mergedData$barPres*10
mergedData$DO.sat= calc_DO_sat(mergedData$NEON.temp, mergedData$barPres)
mergedData$barPres= mergedData$barPres/10 #convert back for co2 sat calculations

#load atmospheric CO2 data
load(paste0(getwd(),'/NEON Sensor Data/KONA.atm.isoCO2.RData'))

mergedData= merge(mergedData, atm, all.x = T)
mergedData$co2.atm.ppmv= na.spline(mergedData$co2.atm.ppmv, maxgap = 24)

mergedData$site= 'KING'

allData= rbind(allData, mergedData)

#convert DO from mg to uM
allData$dissolvedOxygen= allData$dissolvedOxygen * 1000 / 31.999
allData$DO.sat= allData$DO.sat * 1000 / 31.999

#calculate CO2.sat
#Henry's law constants and temperature dependence from Sander (2015)
ckHCO2 = 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
cdHdTCO2 = 2400 #K, range: 2300 - 2600
cT0 = 298.15 #Henry's law constant T0

allData$CO2.sat = (ckHCO2 * exp(cdHdTCO2*(1/(allData$NEON.temp + 273.15) - 1/cT0))) * 
  allData$co2.atm.ppmv * allData$barPres

#calculate departures:
allData$O2.departure<-allData$dissolvedOxygen - allData$DO.sat
allData$CO2.departure<-allData$CO2.uM.corr - allData$CO2.sat
#change to if-else if not all sites are using corr data
allData$CO2.departure.raw<-allData$CO2.uM - allData$CO2.sat
allData$site=as.factor(allData$site)

save(allData, file = 'departures.test.data.RData')
#save(allData, file = 'DeparturesInput.AllSites.RData')

#### PLOT AND CALCULATE CLOUD METRICS: INTERANNUAL ####
load('departures.test.data.RData')
#load('DeparturesInput.AllSites.RData')
siteName='KING'
filtered= allData[allData$site==siteName,]

#Visualize the point cloud
ggplot(data = allData, aes(x=CO2.departure.raw, y=O2.departure, color=co2.atm.ppmv)) +
  geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size=0.5) + labs(title = 'All Sites- uncorrected data') + theme_bw()
ggsave(filename = paste0(getwd(),'/Departures Plots/Departures.alldata.rawCO2.jpg'), width = 6, height = 6)
ggplot(data = filtered, aes(x=CO2.departure.raw, y=O2.departure, color=solar.time)) +
  geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size=0.5) + labs(title = paste0(siteName,'- uncorrected CO2')) + theme_bw()
ggsave(filename = paste0(getwd(),'/Departures Plots/', siteName, '.departures.alldata.jpg'), width = 4.5, height = 4)

#Type II regression used for calculating (some) cloud metrics
mod= lmodel2(O2.departure~CO2.departure, data = filtered)
mod

#Centroid
#time averages of O2 and CO2 departure values
centroid.x= mean(filtered$CO2.departure, na.rm=T)
centroid.y= mean(filtered$O2.departure, na.rm=T)

#Offset
#Sum of CO2 and O2 departures averages
#NOT ACTUALLY THE DISTNACE FROM THE 1:-1 LINE
offset= centroid.x+centroid.y

#Stretch
#Major axis length of 95% covariance error ellipse
#where p(s<=5.991)=0.95 on a chi-square distribution 
stretch= 2 * sqrt(5.991 * mod$eigenvalues[1])

#Width
#minor axis length of 95% covariance error ellipse
width= 2 * sqrt(5.991 * mod$eigenvalues[2])

#1/|slope|
#inverse of the absolute slope of type II linear regression
#currently using slope from SMA regression
inv.slope= 1 / abs(mod$regression.results[3,3])

#### PLOT AND CALCULATE CLOUD METRICS: DIEL ####
load('departures.test.data.RData')
#load('DeparturesInput.AllSites.RData')

allData$date= date(allData$solar.time) #update to datetime

siteName= 'KING'
filtered= filter(allData, site==siteName & !is.na(O2.departure) & !is.na(CO2.departure.raw)) #update to CO2.departure

output= as.data.frame(unique(filtered$date))
#choose plotting lines to use at end of loop depending on how much info you want
for (i in 1:nrow(output)) {
  d= filter(filtered, date==output[i,1])
  #Type II regression used for calculating (some) cloud metrics
  mod= lmodel2(O2.departure~CO2.departure.raw, data = d)
  #Centroid
  centroid.x= mean(d$CO2.departure.raw, na.rm=T)
  centroid.y= mean(d$O2.departure, na.rm=T)
  #Offset
  offset= centroid.x+centroid.y
  #Stretch
  stretch= 2 * sqrt(5.991 * mod$eigenvalues[1])
  #Width
  width= 2 * sqrt(5.991 * mod$eigenvalues[2])
  #1/|slope|
  inv.slope= 1 / abs(mod$regression.results[3,3])
  #Add to output dataframe:
  output[i,c(2:8)]= c(nrow(d), centroid.x, centroid.y, offset, stretch, width, inv.slope)
  #Visualize:
  dp= ggplot(d, aes(x=CO2.departure.raw, y=O2.departure, color=solar.time)) +
    geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_abline(slope = mod$regression.results[3,3], intercept = mod$regression.results[3,2], color = 'blue') +
    geom_point() + annotate(geom = 'point', x= centroid.x, y= centroid.y, color='red', shape=4, size=3) +
    labs(title = paste0(output[i,1],' (blue= slope, x= centroid)')) + theme_bw()
  # tp= ggplot(data = d, aes(x=CO2.departure.raw, y=O2.departure, color=eosense.temp)) + #change to neon temp
  #   geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  #   geom_point() + theme_bw()
  # qp= ggplot(data = d, aes(x=CO2.departure.raw, y=O2.departure, color=discharge)) +
  #   geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  #   geom_point() + theme_bw()
  # p= plot_grid(dp, tp, qp, ncol = 1)
  # ggsave(plot = p, filename = paste0(getwd(),'/Departures Results/Plots/Diel/',siteName,'/',siteName,'.departures.',output[i,1],'.jpg'), width = 5, height = 12)
  ggsave(plot = dp, filename = paste0(getwd(),'/Departures Results/Plots/Diel/',siteName,'/',siteName,'.departures.',output[i,1],'.jpg'), width = 5, height = 4)
}

names(output)[1:8]= c('date','n','centroid.x','centroid.y','offset','stretch','width','inverse.slope')
save(output, file = paste0(getwd(),'/Departures Results/',siteName,'.diel.departures.RData'))

####NON- LOOPED VERSION:####
d= filter(filtered, date==unique(filtered$date)[2])

#Type II regression used for calculating (some) cloud metrics
mod= lmodel2(O2.departure~CO2.departure.raw, data = d)
#Centroid
centroid.x= mean(filtered$CO2.departure.raw, na.rm=T)
centroid.y= mean(filtered$O2.departure, na.rm=T)
#Offset
offset= centroid.x+centroid.y
#Stretch
stretch= 2 * sqrt(5.991 * mod$eigenvalues[1])
#Width
width= 2 * sqrt(5.991 * mod$eigenvalues[2])
#1/|slope|
inv.slope= 1 / abs(mod$regression.results[3,3])

#Visualize the point cloud
ggplot(data = d, aes(x=CO2.departure.raw, y=O2.departure, color=solar.time)) +
  geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_abline(slope = mod$regression.results[3,3], intercept = mod$regression.results[3,2], color = 'blue') +
  geom_abline(slope = mod$regression.results[1,3], intercept = mod$regression.results[1,2], color = 'red') +
  geom_point() + labs(title = 'red= type I, blue= type II regression') + theme_bw()


#Plotting trends in metrics over time:
o= ggplot(output, aes(x=date, y=offset)) + geom_point() + ggtitle(siteName) + theme_bw() + theme(axis.title.x = element_blank())
s= ggplot(output, aes(x=date, y=stretch)) + geom_point() + theme_bw() + theme(axis.title.x = element_blank())
w= ggplot(output, aes(x=date, y=width)) + geom_point() + theme_bw() + theme(axis.title.x = element_blank())
plot_grid(o, s, w, ncol = 1, rel_heights = c(1.01,1,1))

output$year= as.factor(year(output$date))

a= ggplot(filter(output, year=='2022'), aes(x=centroid.x, y=centroid.y, color=date)) +
  geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_point() +
  labs(title = paste0(siteName, ' 2022 daily centroids')) + theme_bw()
b= ggplot(filter(output, year=='2023'), aes(x=centroid.x, y=centroid.y, color=date)) +
  geom_abline(slope = -1, intercept = 0) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + geom_point() +
  labs(title = paste0('2023 daily centroids')) + theme_bw()
plot_grid(a, b, ncol = 1)
