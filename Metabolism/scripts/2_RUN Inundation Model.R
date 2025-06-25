##### For ND Incubations #####

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")


##################################################
## ND Incubation Days
##################################################



#### ND July 15 #####

nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.09

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.267 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (101.453*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################



############################################
### [3] SET working drive & LOAD merged data
############################################

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/15/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/16/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230715.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230715.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




#########################################################


#### ND July 15 ####


# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.06

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.411 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (102.233*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

###################

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="09/30/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/01/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230930.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230930.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




#########################################################

#### ND 3 November ####


# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.07

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.18 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (103.203*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###


# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/03/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/04/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231103.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231103.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")


#########################################################

#### ND 3 February ####
## Feb 25th didn't work out ##


# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.15

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.98 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (101.988*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)


# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="02/26/2024", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="02/27/2024", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20240226.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20240226.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")



##############################################################################################





#########################################
# DK Incubation days
#########################################


#### DK 11 July ####
### Note: no days worked from 12-15 ###


# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.09

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.461 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (101.465*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################


# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882307, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###


# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/11/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/12/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20230711.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20230711.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")



##############################################################################################


#### DK 30 September ####

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.10

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.468 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (102.646*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###


# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882307, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###


# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="09/30/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/01/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20230930.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20230930.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")


#################################################################


#### DK 26 February ####
## Feb 25th didn't work, 26th is alright ##

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.13

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.572 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (101.988*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################


# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882307, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="02/26/2024", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="02/27/2024", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20240226.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20240226.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")








##################################################################################################
#### Other inundated days
##################################################################################################


#### ND 24 July ####

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.14

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.557 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (102.024*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 24 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/24/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/25/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230724.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230724.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




###################################################################################################




##### DK 07/24/2023 #####

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.15

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.690 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (102.024*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 24 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/24/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/25/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20230724.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20230724.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")











###################################################################################################




##### ND 06/08/2023 #####

### When sites are inundated PRIOR TO DRAWDOWN ###


# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.12

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.639 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (100.712*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###



############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="06/08/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="06/09/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230608.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230608.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




###################################################################################################



##### DK 06/08/2023 #####


##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.09

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.433 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (100.712*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="06/08/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="06/09/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20230608.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20230608.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")





###################################################################################################



##### ND 10/20/2023 #####


##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.095

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.349 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (100.673*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="10/20/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/21/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231020.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231020.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




###################################################################################################





##### ND 10/22/2023 #####

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.05

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.360 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
k600 <- 0.01 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (100.909*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="10/22/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/23/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231022.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231022.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")







###################################################################################################






#### DK 10/21/2023 ####

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.07

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.261 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (99.900*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="10/21/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/22/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20231021.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20231021.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")




###################################################################################################





#### DK 10/22/2023 ####

##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Last updated: 20240612 by ERH to include BP data (not calc from elev)
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R file FIRST!

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper
nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.03

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!
Wetland <- "DK"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.212 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
# explored "test runs" for DK with zmix = 0.7, 1.0, and 1.3
k600 <- 0.015 # m/h from LGR (seems low!) - **also run all days for k600 = 0.04 from wind model**
bp <- (100.909*7.50062) #101.453 kPa on 24 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model_Updated")

# Load data TEST for DK metab
DelMET <- read.csv("DK_DO_2023.csv")
head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime<-chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))
# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=39.0,  longobs=75.4, longstd= 75, year="2023-01-01")
# checck that modeled light makes sense
plot(DelMET$dtime, DelMET$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# another place to review data patterns more broadly if you haven't already....
# plot oxygen data
plot(DelMET$dtime, DelMET$oxy)
# plot temp data
plot(DelMET$dtime, DelMET$temp)
# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(DelMET$dtime, Kcor(DelMET$temp, k600))

### / END working data section ###

############################################
### [4] RUN metabolism model!!
############################################

# For ND test runs to compare with bioassays:
# July 24 2023 (assays on 18 July)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 9 JUNE

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="10/22/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="10/23/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(Excel)
# combine with any previous output so it's all in one place
Excel=rbind(Excel,m.out)
# replace with correct column names
names(Excel) <- Colnames
# check that this worked!
Excel

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="DK.20231022.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="DK.20231022.O2.pdf")
lakeMETplot (data=data, bp=bp, zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(Excel, file="Excel.csv")






