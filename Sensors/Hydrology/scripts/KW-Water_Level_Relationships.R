#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Water Level Relationships
#Coder: Katie Wardinski
#Created: 2024-05-07
#Purpose: Use OW (Flux restored) stage data to estimate ND and TS stages 
#during rain event sampling in Dec 2023
#Notes: Modified from "TS_ND_Stage_Estimates.R" in Synoptic_students Github
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load relevant packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)
library(raster)
library(patchwork)
library(plotly)
library(ggrepel)
library(ggpmisc)

#set theme classic
theme_set(theme_classic())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Read water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 2.1 Read water level data -----------------------------------
#daily mean water level 2019 through fall 2022 updated by Nick 
#WL_2019_2022 <- read_csv("Water_Level_Data/dly_mean_output_NC_2019_2022.csv") 

#daily mean water level Jackson lane only 2022 to 2023
#WL_2022_2023 <- read_csv("Water_Level_Data/JL_SW_dailyWL_2022_2023.csv")

#2019 - 2022 15 minute water level data
hf_WL_2019_2022 <- read_csv("Water level & precip/data/output_JM_2019_2022.csv") 

#2022-23 15 minute water level data for ND and TS
ND_hf_WL_2022_2023 <- read_csv("Water_Level_Data/ND-SW_WL2022_2023.csv")
TS_hf_WL_2022_2023 <- read_csv("Water_Level_Data/TS-SW_WL2022_2023.csv")


## 2.2 Read in Flux restored OW and ND estimates from Michael W -----------------
OW_hf_WL_2021_2024 <- read_csv("Water level & precip/data/ND_stage_estimate_4-28-21_to_2-29-24.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Format water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##3.1 High frequency data ----------------------------------------
#fix column names, date formate, and filter to ND/TS

#full data set 2019-2022
hf_WL_2019_2022b <- hf_WL_2019_2022 %>% 
  rename(Site_ID = Site_Name,
         Stage = waterLevel) %>% 
  filter(Site_ID %in% c("ND-SW","ND-UW1","ND-UW2","TS-SW","BD-CH","TS-UW1")) %>% 
  dplyr::select(Timestamp,Site_ID,Stage)

#ND-SW 2022-2023
ND_hf_WL_2022_2023b <- ND_hf_WL_2022_2023 %>%  
  dplyr::select(Timestamp,Site_ID,Stage)

ND_hf_WL_2022_2023b$Timestamp <- mdy_hm(ND_hf_WL_2022_2023b$Timestamp)

#TS-SW 2022-2023
TS_hf_WL_2022_2023b <- TS_hf_WL_2022_2023 %>% 
  dplyr::select(Timestamp,Site_ID,Stage)

TS_hf_WL_2022_2023b$Timestamp <- mdy_hm(TS_hf_WL_2022_2023b$Timestamp)

# Bind data together
hf_WLa <- rbind(hf_WL_2019_2022b,ND_hf_WL_2022_2023b)
hf_WL <- rbind(hf_WLa,TS_hf_WL_2022_2023b)

##***there's a gap between April and October 2022***###

#Format Michael W data to match other high freq data
OW_hf_WL_2021_2024b <- OW_hf_WL_2021_2024 %>% 
  rename(Stage = OW_SW_stage_m) %>% dplyr::select(Timestamp,Stage) %>% mutate(Site_ID = "FR-OW")
OW_hf_WL_2021_2024b$Timestamp <- mdy_hm(OW_hf_WL_2021_2024b$Timestamp)

#Bind Michael OW stage data
hf_WL_OW <- rbind(hf_WL,OW_hf_WL_2021_2024b)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Plot water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 4.1 All data ----------------------------------------------
hf_WL_OW %>% 
  ggplot(aes(Timestamp,Stage,col=Site_ID))+
  geom_line()+
  ylab("Water Level (m)")+
  xlab("")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

## 4.2 SW vs FR-OW relationships --------------------------------
hf_WL_OW_wide <- pivot_wider(hf_WL_OW,
                             names_from = Site_ID,
                             values_from = Stage)

colnames(hf_WL_OW_wide) <- c("Timestamp","BD_CH","ND_UW1","ND_UW2","ND_SW","TS_SW","TS_UW1","FR_OW")

hf_WL_OW_wide$Year <- format(hf_WL_OW_wide$Timestamp, format="%Y")
hf_WL_OW_wide$Month <- format(hf_WL_OW_wide$Timestamp, format="%Y-%m")

#ND-SW vs FR-SW
ND_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_SW,FR_OW) %>% drop_na()

#ggplot(data=ND_FR,aes(x=FR_OW,y=ND_SW,col=Year))+
ggplot(data=ND_FR,aes(x=FR_OW,y=ND_SW))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-SW (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: ND-SW = 1.7(FR-OW) - 0.13 
#(R2 = 0.92)

ND_SW_model <- lm(ND_SW ~ FR_OW, data = ND_FR) 
summary(ND_SW_model)
#Using all data in lm: ND-SW = 1.700082(FR-OW) - 0.133499 

#TS-SW vs FR-SW
TS_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_SW,FR_OW) %>% drop_na()

#ggplot(data=TS_FR,aes(x=FR_OW,y=TS_SW,col=Year))+
ggplot(data=TS_FR,aes(x=FR_OW,y=TS_SW))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-SW (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: TS-SW = 2.4(FR-OW) - 0.75 
#(R2 = 0.98)

TS_SW_model <- lm(TS_SW ~ FR_OW, data = TS_FR) 
summary(TS_SW_model )
#Using all data in lm: TS-SW = 2.384488 (FR-OW) - 0.745437 


## 4.3 GW vs FR-OW relationships ----------------------------------
#ND-UW vs FR-SW
ND_GW_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_UW1,ND_UW2,FR_OW) %>% drop_na()

#ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW1,col=Year))+
ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW1))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW1 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: ND-UW1 = 2.2(FR-OW) - 1.9 
#(R2 = 0.98)

ND_UW1_model <- lm(ND_UW1 ~ FR_OW, data = ND_GW_FR) 
summary(ND_UW1_model)
#Using all data in lm: ND-UW1 = 2.164792(FR-OW) - 1.904806 


#ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW2,col=Year))+
ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW2))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW2 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: ND-UW2 = 2.1(FR-OW) - 1.7
#(R2 = 0.96)

ND_UW2_model <- lm(ND_UW2 ~ FR_OW, data = ND_GW_FR) 
summary(ND_UW2_model)
#Using all data in lm: ND-UW2 = 2.105357(FR-OW) - 1.685334 

#TS-UW vs FR-SW
TS_GW_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_UW1,BD_CH,FR_OW) %>% drop_na()

#TS-UW1 / BD-CH
#ggplot(data=TS_GW_FR,aes(x=FR_OW,y=BD_CH,col=Year))+
ggplot(data=TS_GW_FR,aes(x=FR_OW,y=BD_CH))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW1 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: TS-UW1/BD-CH = 3(FR-OW) - 1.7
#(R2 = 0.96)
#has a break point at FR > 0.6 m

TS_UW1_model <- lm(BD_CH ~ FR_OW, data = TS_GW_FR) 
summary(TS_UW1_model)
#Using all data in lm: TS_UW1 = 2.967507(FR-OW) - 1.725438 

#TS-UW2
#ggplot(data=TS_GW_FR,aes(x=FR_OW,y=TS_UW1,col=Year))+
ggplot(data=TS_GW_FR,aes(x=FR_OW,y=TS_UW1))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW2 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Using all data: TS-UW2 = 3(FR-OW) - 2.3
#(R2 = 0.96)

TS_UW2_model <- lm(TS_UW1 ~ FR_OW, data = TS_GW_FR) 
summary(TS_UW2_model)
#Using all data in lm: TS_UW2 = 3.039054(FR-OW) - 2.297439 


## 4.4 SW vs GW relationships --------------------------

#ND SW vs ND-UW1 and ND-UW2
ND_all <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_SW,ND_UW1,ND_UW2) %>% drop_na()


ggplot(data=ND_all,aes(x=ND_SW,y=ND_UW1,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW1 (m)")+
  xlab("ND-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=ND_all,aes(x=ND_SW,y=ND_UW2,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW2 (m)")+
  xlab("ND-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS SW vs TS-UW1 and TS-UW2
TS_all <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_SW,BD_CH,TS_UW1) %>% drop_na()


ggplot(data=TS_all,aes(x=TS_SW,y=BD_CH,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW1 (m)")+
  xlab("TS-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=TS_all,aes(x=TS_SW,y=TS_UW1,col=Month))+
  geom_point()+
  #geom_smooth(method='lm')+
  #stat_regline_equation(label.x = 0.1)+
  #stat_cor(label.x = 0.3)+
  ylab("TS-UW2 (m)")+
  xlab("TS-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Estimate Water Levels -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 5.1 Estimate water levels ----------------------------------
FR_SW <- OW_hf_WL_2021_2024b %>% 
  rename(FR_OW = Stage) %>% 
  dplyr::select(Timestamp,FR_OW) 

Stage_Estimates <- FR_SW %>% mutate(
  #lm: ND-SW = 1.700082(FR-OW) - 0.133499   
  ND_SW_Estimated = (FR_OW * 1.700082) - 0.133499,
  #lm: TS-SW = 2.384488 (FR-OW) - 0.745437
  TS_SW_Estimated = (FR_OW * 2.384488) - 0.745437,
  #lm: ND-UW1 = 2.164792(FR-OW) - 1.904806 
  ND_UW1_Estimated = (FR_OW * 2.164792) - 1.904806,
  #lm: ND-UW2 = 2.105357(FR-OW) - 1.685334 
  ND_UW2_Estimated = (FR_OW * 2.105357) - 1.685334,
  #lm: TS_UW1 = 2.967507(FR-OW) - 1.725438 
  TS_UW1_Estimated = (FR_OW * 2.967507) - 1.725438,
  #lm: TS_UW2 = 3.039054(FR-OW) - 2.297439 
  TS_UW2_Estimated = (FR_OW * 3.039054) - 2.297439) 

#Export estimated water levels
write.csv(Stage_Estimates,"ND_TS_Stage_Estimates.csv")

## 5.2 Plot estimated data ------------------------
Stage_Estimates %>% 
  filter(Timestamp > "2023-08-01 00:00:00" & Timestamp < "2023-12-31 12:00:00") %>% 
  ggplot()+
  geom_line(aes(Timestamp,ND_SW_Estimated,col="ND-SW"))+
  geom_line(aes(Timestamp,TS_SW_Estimated,col="TS-SW"))+
  geom_line(aes(Timestamp,ND_UW1_Estimated,col="ND-UW1"))+
  geom_line(aes(Timestamp,ND_UW2_Estimated,col="ND-UW2"))+
  geom_line(aes(Timestamp,TS_UW1_Estimated,col="TS-UW1"))+
  geom_line(aes(Timestamp,TS_UW2_Estimated,col="TS-UW2"))+
  ylab("Water Level (m)")+
  xlab("")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

## 5.3 Plot estimated vs measured data for 2019 - 2022 ----------------

#ND-SW
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,ND_SW_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,ND_SW,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("ND-SW")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#ND-UW1
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,ND_UW1_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,ND_UW1,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("ND-UW1")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#ND-UW2
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,ND_UW2_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,ND_UW2,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("ND-UW2")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS-SW
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,TS_SW_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,TS_SW,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("TS-SW")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS-UW1 (BD-CH)
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,TS_UW1_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,BD_CH,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("TS-UW1")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS-UW2 (TS-UW1)
ggplot()+
  geom_line(data = Stage_Estimates,aes(Timestamp,TS_UW2_Estimated,col="Estimated"))+
  geom_line(data= hf_WL_OW_wide, aes(Timestamp,TS_UW1,col="Measured"))+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("TS-UW1")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Estimated water level during rain event ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ND
Stage_Estimates %>% 
  filter(Timestamp > "2023-12-09 00:00:00" & Timestamp < "2023-12-13 12:00:00") %>% 
  ggplot()+
  geom_line(aes(Timestamp,ND_SW_Estimated,col="ND-SW"),size=1)+
  geom_line(aes(Timestamp,ND_UW1_Estimated,col="ND-UW1"),size=1)+
  geom_line(aes(Timestamp,ND_UW2_Estimated,col="ND-UW2"),size=1)+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("ND Estimated WL Data")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14))

#TS
Stage_Estimates %>% 
  filter(Timestamp > "2023-12-09 00:00:00" & Timestamp < "2023-12-13 12:00:00") %>% 
  ggplot()+
  geom_line(aes(Timestamp,TS_SW_Estimated,col="TS-SW"),size=1)+
  geom_line(aes(Timestamp,TS_UW1_Estimated,col="TS-UW1"),size=1)+
  geom_line(aes(Timestamp,TS_UW2_Estimated,col="TS-UW2"),size=1)+
  ylab("Water Level (m)")+
  xlab("")+
  ggtitle("TS Estimated WL Data")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS and ND SW only
Stage_Estimates %>% 
  filter(Timestamp > "2023-12-09 00:00:00" & Timestamp < "2023-12-12 12:00:00") %>% 
  ggplot()+
  geom_line(aes(Timestamp,TS_SW_Estimated,col="TS-SW"),size=1)+
  geom_line(aes(Timestamp,ND_SW_Estimated,col="ND-SW"),size=1)+
  geom_vline(xintercept=ymd_hms("2023-12-10 13:00:00"), linetype="dashed", color = "black")+
  geom_vline(xintercept=ymd_hms("2023-12-11 10:00:00"), linetype="dashed", color = "black")+
  ylab("Water Level (m)")+
  xlab("")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#7.0 Estimated head gradients during rain event ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculate gradients between wells
Gradients <- Stage_Estimates %>% 
  filter(Timestamp > "2023-12-09 00:00:00" & Timestamp < "2023-12-13 12:00:00") %>% 
  mutate(TS_SW_UW1 = TS_SW_Estimated - TS_UW1_Estimated,
         TS_SW_UW2 = TS_SW_Estimated - TS_UW2_Estimated,
         ND_SW_UW1 = ND_SW_Estimated - ND_UW1_Estimated,
         ND_SW_UW2 = ND_SW_Estimated - ND_UW2_Estimated)

#Plot ND gradients
Gradients %>% 
  ggplot()+
  geom_line(aes(Timestamp,ND_SW_UW1,col="SW to UW1"),size=1)+
  geom_line(aes(Timestamp,ND_SW_UW2,col="SW to UW2"),size=1)+
  ylab("Gradient (SW - UW) (m)")+
  xlab("")+
  ggtitle("ND")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14))

#Plot TS gradients
Gradients %>% 
  ggplot()+
  geom_line(aes(Timestamp,TS_SW_UW1,col="SW to UW1"),size=1)+
  geom_line(aes(Timestamp,TS_SW_UW2,col="SW to UW2"),size=1)+
  ylab("Gradient (SW - UW) (m)")+
  xlab("")+
  ggtitle("TS")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14))
