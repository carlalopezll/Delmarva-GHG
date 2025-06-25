###### Metabolism Model Data Analysis #######

setwd("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model")


library(readxl)
Model_Results <- read_excel("~/Delmarva Wetlands Data/Personal Research Data/Metabolism/Metabolism_Model/Model_Results.xlsx")

library(stringr)
library(dplyr)
Model_Results$Date <- as.Date(Model_Results$Date)
Model_Results_Limited1 <- Model_Results[Model_Results$Date == "2023-07-15",]
Model_Results_Limited2 <- Model_Results[Model_Results$Date == "2023-09-30",]
Model_Results_Limited3 <- Model_Results[Model_Results$Date == "2023-11-03",]


Model_Results_Limited <- combine(Model_Results_Limited1, Model_Results_Limited2, Model_Results_Limited3)




NDJuly_Model_Measurements <- data.frame(Date = numeric(3),
                                                 Duration.hrs = numeric(3),
                                                 Site = numeric(3),
                                                 DO.gm2d =  numeric(3),
                                                 DO.mgLhr =  numeric(3),
                                                 HoursFactor = numeric(3))

NDJuly_Model_Measurements$Date <- as.Date("2023-07-15")

NDJuly_Model_Measurements$Duration.hrs <- 24
NDJuly_Model_Measurements$Duration.hrs <- as.numeric(NDJuly_Model_Measurements$Duration.hrs)

NDJuly_Model_Measurements$Site <- "ND-SW"

NDJuly_Model_Measurements$Productivity_Measurement[1] <- "NEP (Net Ecosystem Production)"
NDJuly_Model_Measurements$Productivity_Measurement[2] <- "ER (Ecosystem Respiration)"
NDJuly_Model_Measurements$Productivity_Measurement[3] <- "GPP (Gross Primary Production)"
NDJuly_Model_Measurements$Productivity_Measurement <- as.factor(NDJuly_Model_Measurements$Productivity_Measurement)

NDJuly_Model_Measurements$DO.gm2d[1] <- ((Model_Results_Limited$GPP.50[1]) + (Model_Results_Limited$ER.50[1]))
NDJuly_Model_Measurements$DO.gm2d[2] <- abs(Model_Results_Limited$ER.50[1])
NDJuly_Model_Measurements$DO.gm2d[3] <- abs(Model_Results_Limited$GPP.50[1])
NDJuly_Model_Measurements$DO.gm2d <- as.numeric(NDJuly_Model_Measurements$DO.gm2d)

NDJuly_Model_Measurements$DO.mgLhr[1] <- ((NDJuly_Model_Measurements$DO.gm2d[1])*1000)*(1/(.30*1000))*(1/24)
NDJuly_Model_Measurements$DO.mgLhr[2] <- ((NDJuly_Model_Measurements$DO.gm2d[2])*1000)*(1/(.30*1000))*(1/24)
NDJuly_Model_Measurements$DO.mgLhr[3] <- ((NDJuly_Model_Measurements$DO.gm2d[3])*1000)*(1/(.30*1000))*(1/24)
NDJuly_Model_Measurements$DO.mgLhr <- as.numeric(NDJuly_Model_Measurements$DO.mgLhr)

NDJuly_Model_Measurements$HoursFactor <- "Sensor Data"
NDJuly_Model_Measurements$HoursFactor <- as.factor(NDJuly_Model_Measurements$HoursFactor)

###


NDSept_Model_Measurements <- data.frame(Date = numeric(3),
                                        Duration.hrs = numeric(3),
                                        Site = numeric(3),
                                        DO.gm2d =  numeric(3),
                                        DO.mgLhr =  numeric(3),
                                        HoursFactor = numeric(3))

NDSept_Model_Measurements$Date <- as.Date("2023-09-30")

NDSept_Model_Measurements$Duration.hrs <- 24
NDSept_Model_Measurements$Duration.hrs <- as.numeric(NDSept_Model_Measurements$Duration.hrs)

NDSept_Model_Measurements$Site <- "ND-SW"

NDSept_Model_Measurements$Productivity_Measurement[1] <- "NEP (Net Ecosystem Production)"
NDSept_Model_Measurements$Productivity_Measurement[2] <- "ER (Ecosystem Respiration)"
NDSept_Model_Measurements$Productivity_Measurement[3] <- "GPP (Gross Primary Production)"
NDSept_Model_Measurements$Productivity_Measurement <- as.factor(NDSept_Model_Measurements$Productivity_Measurement)


NDSept_Model_Measurements$DO.gm2d[1] <- ((Model_Results_Limited$GPP.50[4]) + (Model_Results_Limited$ER.50[4]))
NDSept_Model_Measurements$DO.gm2d[2] <- abs(Model_Results_Limited$ER.50[4])
NDSept_Model_Measurements$DO.gm2d[3] <- abs(Model_Results_Limited$GPP.50[4])
NDSept_Model_Measurements$DO.gm2d <- as.numeric(NDSept_Model_Measurements$DO.gm2d)

NDSept_Model_Measurements$DO.mgLhr[1] <- ((NDSept_Model_Measurements$DO.gm2d[1])*1000)*(1/(.30*1000))*(1/24)
NDSept_Model_Measurements$DO.mgLhr[2] <- ((NDSept_Model_Measurements$DO.gm2d[2])*1000)*(1/(.30*1000))*(1/24)
NDSept_Model_Measurements$DO.mgLhr[3] <- ((NDSept_Model_Measurements$DO.gm2d[3])*1000)*(1/(.30*1000))*(1/24)
NDSept_Model_Measurements$DO.mgLhr <- as.numeric(NDSept_Model_Measurements$DO.mgLhr)


NDSept_Model_Measurements$HoursFactor <- "Sensor Data"
NDSept_Model_Measurements$HoursFactor <- as.factor(NDSept_Model_Measurements$HoursFactor)


###

NDNov_Model_Measurements <- data.frame(Date = numeric(3),
                                       Duration.hrs = numeric(3),
                                       Site = numeric(3),
                                       DO.gm2d =  numeric(3),
                                       DO.mgLhr =  numeric(3),
                                       HoursFactor = numeric(3))

NDNov_Model_Measurements$Date <- as.Date("2023-11-03")

NDNov_Model_Measurements$Duration.hrs <- 24
NDNov_Model_Measurements$Duration.hrs <- as.numeric(NDNov_Model_Measurements$Duration.hrs)

NDNov_Model_Measurements$Site <- "ND-SW"

NDNov_Model_Measurements$Productivity_Measurement[1] <- "NEP (Net Ecosystem Production)"
NDNov_Model_Measurements$Productivity_Measurement[2] <- "ER (Ecosystem Respiration)"
NDNov_Model_Measurements$Productivity_Measurement[3] <- "GPP (Gross Primary Production)"
NDNov_Model_Measurements$Productivity_Measurement <- as.factor(NDNov_Model_Measurements$Productivity_Measurement)

NDNov_Model_Measurements$DO.gm2d[1] <- ((Model_Results_Limited$GPP.50[7]) + (Model_Results_Limited$ER.50[7]))
NDNov_Model_Measurements$DO.gm2d[2] <- abs(Model_Results_Limited$ER.50[7])
NDNov_Model_Measurements$DO.gm2d[3] <- abs(Model_Results_Limited$GPP.50[7])
NDNov_Model_Measurements$DO.gm2d <- as.numeric(NDNov_Model_Measurements$DO.gm2d)

NDNov_Model_Measurements$DO.mgLhr[1] <- ((NDNov_Model_Measurements$DO.gm2d[1])*1000)*(1/(.30*1000))*(1/24)
NDNov_Model_Measurements$DO.mgLhr[2] <- ((NDNov_Model_Measurements$DO.gm2d[2])*1000)*(1/(.30*1000))*(1/24)
NDNov_Model_Measurements$DO.mgLhr[3] <- ((NDNov_Model_Measurements$DO.gm2d[3])*1000)*(1/(.30*1000))*(1/24)
NDNov_Model_Measurements$DO.mgLhr <- as.numeric(NDNov_Model_Measurements$DO.mgLhr)


NDNov_Model_Measurements$HoursFactor <- "Sensor Data"
NDNov_Model_Measurements$HoursFactor <- as.factor(NDNov_Model_Measurements$HoursFactor)



Total_Model_Measurements <- combine(NDJuly_Model_Measurements, NDSept_Model_Measurements, NDNov_Model_Measurements)

Total_Model_Measurements$Season <- c("Summer", "Summer", "Summer", "Early Fall", "Early Fall", "Early Fall", "Late Fall", "Late Fall", "Late Fall")
Total_Model_Measurements$Season <- as.factor(Total_Model_Measurements$Season)




library(ggplot2)
library(forcats)
library(lubridate)
library(tidyverse)

ggplot(data=Total_Model_Measurements, aes(x = fct_relevel(Season, "Summer"), y = DO.mgLhr, fill=Productivity_Measurement)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), width = 0.7) +
  theme_minimal() +
  scale_fill_brewer(palette = "Reds") +
  labs(y = expression(paste("Metabolic Rate (mg ", O[2] ,"/L/hour)")),
       x = "Season") +
  geom_hline(yintercept = 0.00) +
  ylim(-0.5,0.75) +
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


geom_errorbar(ymin=0.5, ymax=1.5, width=0.4, colour="black", alpha=0.9, size=1.3)


Total_Model_Measurements$Chla.Conc <- c("10.41", "10.41", "10.41", "10.50", "10.50", "10.50", "0.26", "0.26", "0.26")
NEP_Model_Measurements <- Total_Model_Measurements[Total_Model_Measurements$Productivity_Measurement == "NEP (Net Ecosystem Production)",]
GPP_Model_Measurements <- Total_Model_Measurements[Total_Model_Measurements$Productivity_Measurement == "GPP (Gross Primary Production)",]
ER_Model_Measurements <- Total_Model_Measurements[Total_Model_Measurements$Productivity_Measurement == "ER (Ecosystem Respiration)",]



lm(Chla.Conc ~ DO.mgLhr, GPP_Model_Measurements)
