############################################
# Estimate metabolism for headwater wetlands
	# LOAD Packages, Functions
	# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
	# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
  # Added light model code from Hall & Hotchkiss 2017 via Yard et al.
	# Last updated: 20240612 by ERH to include BP data (not calc from elev)
	# ** YOU SHOULD NOT NEED TO EVER CHANGE THE LOAD FILE. ONLY CHANGE THE RUN FILES **

############################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

## These are all of the functions you'll need to "RUN" daily models to estimate GPP and ER in ponded water. 
# Unless you want to update any of the functions, you can simply "select all" and "execute" this entire file. 
# BUT: you should have a basic understanding of what each of these functions do so you can explain your methods and results!! -ERH, 2016-04-28

### / END notes ###

############################################
### [2] LOAD R PACKAGES 
############################################

## load R packages needed
library(chron)
library(mcmc)

### / END packages ###

############################################
### [3] LOAD function to calculate O2 SAT
############################################

## Function to calculate oxygen saturation given water temperature and barometric pressure.
	## NOTE: this is updated from earlier versions with a more accurate function. - ERH 2016-04-28

## oxygen saturation. From García and Gordon 1992.
	## calculates mL gas / dm^3 water; the below equation converts to mg/L.    
	## BP in mm Hg. 
	## 1.42905 is 'correct' and accounts for the fact that O2 is a non-ideal gas.  
	## u is the vapor pressure of water

osat<- function(temp, bp) {
	
	tstd<-log((298.15-temp) / (273.15 + temp))
	
	a0<-2.00907
	a1<-3.22014
	a2<-4.0501
	a3<-4.94457
	a4<- -0.256847
	a5<- 3.88767
	
	u<-10^(8.10765-(1750.286/(235+temp)))
	
	sato<-(exp(a0 + a1*tstd + a2*tstd^2 + a3*tstd^3 + a4*tstd^4+a5*tstd^5))*((bp-u)/(760-u))*1.42905
	sato
}

### / END O2 sat function ###

############################################
### [4] LOAD functions for KO2
############################################

# "Kcor" == calculate KO2 from K600, corrected for water temperature 
# units are m/h as for K600; lakeMET below converts from m/hr to m/day with *24
# Kcor = KO2 = schmidt.O2/schmidt.600 * K600 
# as in Jähne et al 1987, Wanninkhof 1992, Staehr et al 2010

scaler <- 0.5 

Kcor <- function (temp,k600) {
	schmidt.O2 <-1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)
	schmidt.O2
	KO2 <- k600/(600/(schmidt.O2))^(scaler)
	KO2
	}
	
### / END K functions ###


########################################################################################

############################################
### [5] LOAD the main METABOLISM FUNCTION
############################################

### This is the pond/lake/wetland metabolism function, called "lakeMET"
### Unknown parameters you solve for are called "MET", and include GPP, ER, and model error (sigma)
### As in Van de Bogert et al 2007; Hotchkiss & Hall 2014; Jonsson et al 2015; Hamdan et al. 2018, 2021

lakeMET <- function(MET,oxy,zmix,temp,light,bp,ts,k600){
  
  GPP <- MET[1]
  ER<- MET[2]
  oxyvar <- log(sd(oxy, na.rm=T))
  sigma <- exp(MET[3])
  sigma.prior <- dnorm(sigma,mean=oxyvar,sd=1,log=TRUE)
  
  metab<-numeric(length(oxy))
  metab[1]<-oxy[1]
  
  # relies on a k600 input value for each day, k600 units are m/h as for Kcor function above
  for (i in 2:length(oxy)) { 
    metab[i]<-metab[i-1] + ((MET[1]/zmix)*(light[i]/sum(light))) + (MET[2]*(ts/zmix)) + ((Kcor(temp[i],k600)*24*ts*(osat(temp[i],bp)-metab[i-1]))/zmix) 
  }
  
  # log likelihood for O2
  loglike <- sum(dnorm(oxy,metab,sigma,log=TRUE))
  
  # prior distributions for GPP and ER
  GPP.prior <- dnorm(GPP,mean=1,sd=1) 
  ER.prior <- dnorm(ER,mean=(-1),sd=1)
  sigma.prior <- dnorm(sigma,mean=oxyvar,sd=1,log=TRUE)
  
  # joint prior for GPP and ER
  prior <- log(ER.prior) + log(GPP.prior) + sigma.prior
  
  # posterior = log likelihood + log prior (or likelihood*prior)
  posterior <- loglike + prior
  posterior
}

### END METROP FUNCTION ###

##################################################

### Function to plot modeled vs measured O2 data ###

lakeMETplot<-function(data, GPP, ER,  bp, ts, zmix, k600) {
  oxy<-data$oxy
  temp<-data$temp
  light<-data$light
  dtime<-as.chron(data$dtime)
  
  metab<-numeric(length(oxy))
  metab[1]<-oxy[1]
  
  for (i in 2:length(oxy)) { 
    metab[i]<-metab[i-1] + ((GPP/zmix)*(light[i]/sum(light))) + (ER*(ts/zmix)) + ((Kcor(temp[i],k600)*24*ts*(osat(temp[i],bp)-metab[i-1]))/zmix) }
  
  plot(dtime,metab, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
  points(dtime,oxy)
  oxymodel<-data.frame (metab, oxy, osat(temp,bp))
  
}

### End plot function ###

##################################################

# automate model output 
# this creates a new data frame, output
# each model run will add a new row using "mout" and "rbind"
# NOTE: this will overwrite "output" file in the working drive if you re-load 
N <- 1
# prior versions included: wind.50=numeric(N), Kwind=numeric(N), 
  #,meanPAR=numeric(N), maxPAR=numeric(N), ....
Excel <- data.frame(Wetland=(character(N)), Date=(character(N)), 
                     GPP.2.5=numeric(N), GPP.50=numeric(N), 
                     GPP.97.5=numeric(N), ER.2.5=numeric(N), 
                     ER.50=numeric(N), ER.97.5=numeric(N), 
                     NEPcalc=numeric(N), Kcor=numeric(N),
                     k600.mh=numeric(N), meanTemp=numeric(N),
                     minTemp=numeric(N), maxTemp=numeric(N), 
                     sdTemp=numeric(N), zmix=numeric(N), 
                     bp=numeric(N), sig.50=numeric(N), 
                     rundtime=(character(N)), accept=numeric(N), 
                     nbatch=numeric(N), ncut=numeric(N),  
                     ntot=numeric(N), scale=numeric(N), 
                     stringsAsFactors=FALSE)
# Create "mout" function to update data frame after each run
mout <- function (data, met.out){
  mout <- length(24)
  mout[1] <- Wetland
  mout[2] <- as.character(data$Date[50])
  mout[3]<- quantile(met.out$batch[(ncut:nbatch),1],0.025)
  mout[4]<-quantile(met.out$batch[(ncut:nbatch),1],0.5)
  mout[5]<-quantile(met.out$batch[(ncut:nbatch),1],0.975)
  mout[6]<-quantile(met.out$batch[(ncut:nbatch),2],0.025)
  mout[7]<-quantile(met.out$batch[(ncut:nbatch),2],0.5)
  mout[8]<-quantile(met.out$batch[(ncut:nbatch),2],0.975)
  mout[9] <-quantile(met.out$batch[(ncut:nbatch),1],0.5)+quantile(met.out$batch[(ncut:nbatch),2],0.5)
  mout[10] <- mean(Kcor(data$temp,k600))
  mout[11] <- k600
  mout[12] <-mean(data$temp)
  mout[13]<- min(data$temp)
  mout[14]<- max(data$temp)
  mout[15] <-sd(data$temp)
  mout[16] <- zmix
  mout[17] <- bp
  mout[18]<-quantile(met.out$batch[(ncut:nbatch),3],0.5)
  mout[19] <- as.character(Sys.time())
  mout[20] <-met.out$accept
  mout[21] <- nbatch
  mout[22] <- ncut
  mout[23] <- nbatch-ncut
  mout[24] <- scale
  mout
}
# End mout function

##################################################
##################################################

############################################
### [6] LIGHT MODEL (if needed)
############################################

## Code as in Hall & Hotchkiss 2017 Methods in Stream Ecology

## now make up light data if you don't have it

## From Yard et al. (2005) Ecological Modelling.  Remember your trig?  
## calculate light as umol photon m-2 s-1.
## Arguments are:  
## time = a date and time input (i.e. a chron object) 
## lat = latitude of field site
## longobs = longitude of field site
## longstd = standard longitude of the field site (NOTE: watch daylight savings time!!!). For PST, longstd is be 120 degrees. But during PDT it is 105 degrees. MST is 105 deg. MDT is 90. 
## year = the year for which you collected data and is entered as "2013-01-01"

# convert degrees to radians
radi<-function(degrees){(degrees*pi/180)}

# function to estimate light
lightest<- function (time, lat, longobs, longstd, year ) {
  
  jday<-as.numeric(trunc(time)-as.numeric(as.Date(year)))
  E<- 9.87*sin(radi((720*(jday-81))/365)) - 7.53*cos(radi((360*(jday-81))/365)) - 1.5*sin(radi((360*(jday-81))/365))
  LST<-as.numeric (time-trunc(time))
  ST<-LST+(3.989/1440)*(longstd-longobs)+E/1440
  solardel<- 23.439*sin(radi(360*((283+jday)/365)))
  hourangle<-(0.5-ST)*360
  theta<- acos( sin(radi(solardel)) * sin(radi(lat)) + cos(radi(solardel)) * cos(radi(lat)) * cos(radi(hourangle)) )
  suncos<-ifelse(cos(theta)<0, 0, cos(theta))
  GI<- suncos*2326
  GI	
  
}
#end of function

####### END light model ####### 

########################################################################################

###########################################
# [7] REFERENCES CITED 
###########################################

## May include old citations related to Kwind and other model versions that are no longer cited above ##

# Cole, J.J. & N.F. Caraco. 1998. Atmospheric exchange of carbon dioxide in a low-wind oligotrophic lake measured by the addition of SF6. Limnology & Oceanography 43: 647-656.

# García, H.E. & L.I. Gordon. 1992. Oxygen solubility in seawater: Better fitting equations. Limnology & Oceanography 37: 1307-1312.

# Hall, R.O. & E.R. Hotchkiss. 2017. Stream Metabolism. Chapter 34 In: Methods in Stream Ecology, volume 2, 3rd Edition. Hauer, F.R.  & G.A. Lamberti, Eds. Academic Press. ISBN: 9780128130476

# Hamdan, M., P. Byström, E.R. Hotchkiss, M.J. Al-Haidarey, J. Ask, & J. Karlsson. 2018. Carbon dioxide stimulates lake primary production. Scientific Reports 8: 10878.

# Hamdan, M., P. Byström, E.R. Hotchkiss, M.J. Al-Haidarey, & J. Karlsson. 2021. An experimental test of climate change effects in Northern lakes: Increasing allochthonous organic matter and warming alters autumn primary production. Freshwater Biology 66: 815–825.

# Hotchkiss, E.R. & R.O. Hall. 2014. High rates of daytime respiration in three streams: Use of d18OO2 and O2 to model diel ecosystem metabolism. Limnology & Oceanography 59: 798-810.

# Jähne, B., et al. 1987. On the parameters influencing air-water gas exchange. Journal of Geophysical Research 92: 1937-1949.

# Jonsson, M. et al. 2015. Climate change modifies the size structure of assemblages of emerging aquatic insects. Freshwater Biology 60: 78-88.

# Staehr, P.A., et al. 2010. Lake metabolism and the diel oxygen technique: State of the science. Limnology & Oceanography: Methods 8: 628-644.

# Vachon, D. & Y.T. Prairie. 2013. The ecosystem size and shape dependence of gas transfer velocity versus wind speed relationships in lakes. Can J Fish Aquat Sci 70: 1757-1764.

# Van de Bogert, M. C., S. R. Carpenter, J. J. Cole, and M. L. Pace. 2007. Assessing pelagic and benthic metabolism using free water measurements. Limnology & Oceanography: Methods 5: 145-155.

# Wannikhof, R. 1992. Relationship between wind speed and gas exchange over the ocean. Journal of Geophysical Research 92: 7373-7387.

# Yard, M.D., et al. 2005. Influence of topographic complexity on solar insolation estimates for the Colorado River, Grand Canyon, AZ. Ecological Modelling 183: 157-172. 

####### / END references cited #######

###################################################################################################################