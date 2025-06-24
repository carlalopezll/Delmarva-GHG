##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
	# Last updated: 2024-05-14 by ERH for Delmarva metab test
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
nbatch=51000

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
## fixed parameters for Delmarva models (with #ed-out other model options)
Wetland <- "ND"
ts <- 0.0104167 #(unit = day; 15 min time interval of O2 data)
zmix <- 0.7 #(unit = m; this is the depth of the water seen by sensor)
    # zmix can be fixed by site or vary daily - code allows daily var below
    # explored "test runs" for ND with zmix = 0.7, 1.0, and 1.3
elev <- 13 #(unit = m; this is the elevation above sea level @ Denton)
#LA <- 8e-05 #km2 # pond area as part of K function below if fixed by site
    # will input K estimates directly for now, but this is how model can estimate
k600 = 4/100 # m/h
    # k600.cmh	k600.md
    # 4.164425187	0.999382094
    # 4.794682401	1.150631726
    # 3.366327736	0.807854028

### / END fixed parameters ###

########################################################################################

############################################
### [2] SET working drive & LOAD merged data
############################################

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Sensors/miniDOT DO/Data") # CHANGE FOR YOUR OWN COMPUTER***

# Load data TEST for ND metab
DelMET <- read.csv("ND_DO_2023.csv")
# DelMET <- read.csv("DK_SW_PME_all.csv")

head(DelMET)

# Replace the chron object for "dtime" to make sure it works with how R considers dates and times
DelMET$dtime <- chron(dates=as.character(DelMET$Date), times=as.character(DelMET$Time))

# check that this worked
DelMET$dtime

## estimate light for Delmarva using lightest function in LOAD file
# Set coordinates for Denton, MD for now **** NEED TO UPDATE FOR EACH SITE
# 38.882287, -75.825708 (Denton, MD)
DelMET$light<- lightest(time=DelMET$dtime, lat=38.8,  longobs=75.8, longstd= 75, year="2023-01-01")
# check that modeled light makes sense
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
# July 15 - 20 2023 (assays on 18 July)
# Nov 1 - 5 2023 (assays on 3 Nov)

###################

### RUN METAB MODEL FOR A SINGLE DATE - 15 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/15/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/16/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230715.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230715.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 16 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/16/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/17/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230716.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230716.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 17 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/17/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/18/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
	pdf(file="ND.20230717.pdf")
	plot(ts(met.out$batch))
	dev.off()
# plot and save modeled vs. measured O2
	pdf(file="ND.20230717.O2.pdf")
	lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
	dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 18 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/18/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/19/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230718.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230718.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 19 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/19/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/20/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230719.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230719.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")


###################

### RUN METAB MODEL FOR A SINGLE DATE - 20 JULY

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="07/20/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="07/21/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20230720.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20230720.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 1 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/01/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/02/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231101.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231101.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 2 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/02/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/03/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231102.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231102.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 3 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/03/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/04/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231103.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231103.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 4 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/04/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/05/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231104.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231104.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### RUN METAB MODEL FOR A SINGLE DATE - 5 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="11/05/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="11/06/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231105.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.20231105.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")


###################

### RUN METAB MODEL FOR A SINGLE DATE - 5 NOV

# ***
# identify subset of data for this model run ** from 00:00 to 02:00 the next day **
data=DelMET[DelMET$dtime>=as.numeric(chron(dates="12/15/2023", times="00:00:00")) & DelMET$dtime<=as.numeric(chron(dates="12/16/2023", times="00:02:00")),]
plot(data$dtime, data$oxy)
plot(data$dtime, data$temp)
plot(data$dtime, data$light) # re-check against met data; leave for now

# RUN metrop function to solve for GPP, ER
# met.out will then give you the summary output for that pond
met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=BPcalc(elev),k600=k600)

# To save this output, write to dataframe
# this called the mout function from the LOAD file
m.out <- mout(data, met.out)
# save correct column names
Colnames <- names(output)
# combine with any previous output so it's all in one place
output=rbind(output,m.out)
# replace with correct column names
names(output) <- Colnames
# check that this worked!
output

# *** update PDF names for site, date
## Save plots and model output (you will use these to decide if the model worked)
# plot and save mcmc output showing model convergence on unknown parameters 
pdf(file="ND.20231215.pdf")
plot(ts(met.out$batch))
dev.off()
# plot and save modeled vs. measured O2
pdf(file="ND.202311215.O2.pdf")
lakeMETplot (data=data, bp=BPcalc(elev), zmix=zmix, k600=k600, ts=ts, GPP = quantile(met.out$batch[(ncut:nbatch),1],0.5), ER = quantile(met.out$batch[(ncut:nbatch),2],0.5))
dev.off()

# save updated output dataframe
# REMEMBER to rename this file in your working drive if you start a new LOAD + RUN
write.csv(output, file="output.csv")

###################

### / END running metabolism models for each date ###

########################################################################################

# *** DON'T FORGET to rename the "output.csv" file in your working drive before you leave this file to start other runs!! It contains information about the model performance that you will need to decide if the model worked. If you lose this file, you will have to re-run all of the above models again!!
write.csv(output, file="DelmarvaMetabTest.ND.csv")

########################################################################################
