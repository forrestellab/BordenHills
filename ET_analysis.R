rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(lubridate)
library(naniar)
library(stringr)
library(reshape2)
library(quantmod)


Dir_Input <- c("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/Outputs_2021/D18/Wavelet/")
setwd(Dir_Input)

ET_BH <- read.csv("ET_daily_BH_2021_vf.csv")

ET_BH$DATE <- as.Date(ET_BH$DATE)
ET_BH$Station <- as.factor(ET_BH$Station)
ET_BH$ET <- as.numeric(ET_BH$ET)

ET_BH <- ET_BH %>% mutate(Treatment = substr(Station,1,2), .after = "Station")



ggplot(data = ET_BH, aes(x = DATE, y = ET, colour = Station)) + geom_line() +
  scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                "springgreen4","springgreen3","springgreen2",
                                "blue","royalblue4","skyblue3")) + 
  scale_x_date(limits = as.Date(c("2021-04-12","2021-09-31")),date_breaks = "month", date_labels = "%b")+
  ylab("ET (mm/day)") + xlab("Day")+labs(title = "Borden Hill Evapotranspiration 2021") +
  
  annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-09-07"), xmax = as.Date("2021-09-09") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-09-07"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW3", size = 3, colour = 'red') +
  
  facet_grid(rows = vars(Treatment)) +
  
  theme_classic()

ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/TC_figures/ET_BH_2021.pdf")



#########################
##### Water Balance #####
#########################

#Irrigation_BH_2021 <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Irrigation Fun/csv/Irrigation_BH_Apr_Jun_2021.csv")
Irrigation_BH_2021 <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Irrigation Fun/csv/BH_2021_Irrigation_All.csv")

  
Dates_x <-str_remove(names(Irrigation_BH_2021),"X")  
Dates <- gsub(".","/",Dates_x,fixed = TRUE)

names(Irrigation_BH_2021) <- as.Date(Dates, format = "%d/%b")
names(Irrigation_BH_2021)[1] <- c("Pixel")  

Irrigation_BH_2021$Pixel <- as.numeric(Irrigation_BH_2021$Pixel)

Irrigation_BH_2021 <- Irrigation_BH_2021 %>% mutate(Station = case_when(
  
  Pixel %in% c(4,13,6,25) ~ "B1R1",
  Pixel %in% c(44,53,46,55) ~ "B1R3",
  Pixel %in% c(58,67,60,69) ~ "B1R4",
  
  Pixel %in% c(14,23,16,25) ~ "B2R1",
  Pixel %in% c(24,33,26,35) ~ "B2R2",
  Pixel %in% c(47,48,49,50) ~ "B2R3",
  
  Pixel %in% c(18,27,20,29) ~ "B3R1",
  Pixel %in% c(34,36,43,45) ~ "B3R2",
  Pixel %in% c(54,56,63,65) ~ "B3R3"
  

), .after = "Pixel")
  
Irrigation_BH_2021 <- Irrigation_BH_2021 %>% filter(!is.na(Station)) 

#Irrigation in minutes
Irrigation_min <- apply(Irrigation_BH_2021[,3:ncol(Irrigation_BH_2021)],2, function (x) hour(lubridate::hm(x))*60 + minute(lubridate::hm(x))) %>% 
  as.data.frame
  
####Constants from the field for the variable drip irrigation
Inter_row = 3.05 #[m] inter-row distancing
Inner_row = 0.53 #[m] distance between drip emmitters 
Disch_emi = 0.91 #[l/h] discharge of the emitters (theoretically 0.25 G/h)
Emi_eff = 0.9    #[%] theoretical efficiency of drip irrigation (FAO 56)

####Constants from the field for the variable fixed system irrigation (only used between the 14 July and 24 July)
Inner_row_fx <- 0.425 #[m] distance between drip emmitters
Disch_emi_fx <- 1.9 #[l/h] discharge of the emitters (theoretically 0.5 G/h)

#Discharge per minute
Disch_emi_min = Disch_emi/60 #[l/min] vdi
Disch_emi_min_fx = Disch_emi_fx/60 #[l/min] fix sys

#Emmitters per m^2
Emi_m2 = 1/Inter_row * 1/Inner_row #[emi/m^2] vdi
Emi_m2_fx = 1/Inter_row * 1/Inner_row_fx #[emi/m^2] fx system

Days_fixed_irrigation <- c("2021-07-14","2021-07-15","2021-07-16","2021-07-17",
                           "2021-07-18","2021-07-19","2021-07-20")

#Irrigation L /(m^2*day) for the variable system
Irrigation_m2_day <- Irrigation_min*Disch_emi_min*Emi_eff*Emi_m2

#Irrigation L /(m^2*day) for the fixed system
Irrigation_m2_day[,str_detect(names(Irrigation_min),Days_fixed_irrigation)] <- Irrigation_min[,str_detect(names(Irrigation_min),Days_fixed_irrigation)]*Disch_emi_min_fx*Emi_eff*Emi_m2_fx 

#Merging dataframes 
BH_Irrigation_2021 <- bind_cols(Irrigation_BH_2021[,c(1:2)],Irrigation_m2_day)
BH_Irrigation_2021 <- BH_Irrigation_2021 %>% select(-Pixel)

BH_Irrigation_2021  <- BH_Irrigation_2021 %>% melt(id.vars = "Station") %>% 
  group_by(Station,variable) %>% summarise(Mean_Irr = mean(value))

BH_Irrigation_2021_acc <-  BH_Irrigation_2021 %>% group_by(Station) %>% summarise(Irri_Acc = cumsum(Mean_Irr))

BH_Irrigation_2021_acc <- add_column(BH_Irrigation_2021_acc, Date = as.Date(BH_Irrigation_2021$variable), .after = "Station") 


ggplot(data = BH_Irrigation_2021_acc, aes(x = Date, y= Irri_Acc, color = Station)) + geom_line() +
  ylab("Accumulated Irrigation (mm)") + labs(title = "Borden Hill Irrigation 2021") +
  annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') + 
  theme_classic()

ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/BH_ET_analysis/Acc_Irrigation.pdf")



##########################################
####### Merging ET and Irrigation tables###
##########################################


ET_BH <- ET_BH %>% mutate(TimeStamp_Plot = paste(DATE,Station,sep = "-"), .before = DATE)

BH_Irrigation_2021 <- BH_Irrigation_2021 %>% mutate(TimeStamp_Plot = paste(variable,Station, sep = "-"), .before = Station)

nico <- merge(ET_BH, BH_Irrigation_2021, by.x = "TimeStamp_Plot", by.y = "TimeStamp_Plot", all.x = TRUE)

nico <- nico %>% arrange(Station.x)

nico$Mean_Irr[is.na(nico$Mean_Irr)] <- 0

nico <- nico %>% mutate(Balance = Mean_Irr-ET)

nico <- nico %>% select(DATE,Station.x,Balance)

nico$Balance <- nico$Balance %>%  as.numeric
nico$Balance[nico$Balance == "NaN"] <- NA
nico <- nico %>% filter(!is.na(Balance))

nico <- nico %>% filter(lubridate::month(DATE)>= 6)
nico$Balance[nico$DATE == "2021-06-01"] <- 0



nico_acc <- nico %>% group_by(Station.x) %>% summarise(Balance_acc = cumsum(Balance))

nico_acc <- add_column(nico_acc, Date = nico$DATE, .before = "Station.x")
names(nico_acc) <- c("Date","Station","Balance_acc")

ggplot(data = nico_acc, aes(x= Date, y= Balance_acc, color = Station)) +
  geom_line()+
  scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                "springgreen4","springgreen3","springgreen2",
                                "blue","royalblue4","skyblue3")) + 
  scale_x_date(limits = as.Date(c("2021-06-01","2021-09-15")),date_breaks = "2 week", date_labels = "%d-%b") +
  ylab("Water Balance (mm)") + xlab("Day")+labs(title = "Borden Hill Water Balance 2021") +
  
  annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-09-07"), xmax = as.Date("2021-09-09") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-09-07"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW3", size = 3, colour = 'red') +
  
  geom_hline(aes(yintercept = 0)) +
ylim(-200,200) + theme_bw()
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/BH_ET_analysis/BH_Water_Balance_v2.pdf")



#########################################
###### Comparison between Kyle's ########
## and Bambach's Data ###################
#########################################



Dir_Input <- c("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/Outputs_2021/D18/Wavelet/")
setwd(Dir_Input)

ET_BH <- read.csv("ET_daily_BH_2021_vf.csv")

ET_BH$DATE <- as.Date(ET_BH$DATE)
ET_BH$Station <- as.factor(ET_BH$Station)
ET_BH$ET <- as.numeric(ET_BH$ET)

ET_BH <- ET_BH %>% mutate(Treatment = substr(Station,1,2), .after = "Station")



##### Reading Kyle's data

ET_RS <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/BordenHills2021_Complete/ETday_Kyle_RS.csv")

names(ET_RS) <- c("Year","DOY","Date","B1R4","B2R3",
                  "B3R3","B1R1","B1R3","B2R1","B2R2",
                  "B3R1","B3R2")

ET_RS$Date <- as.Date(ET_RS$Date,"%m/%d/%y")

ET_RS[,c(4:ncol(ET_RS))] <- lapply((ET_RS[,c(4:ncol(ET_RS))]),as.numeric)
  
ET_RS <- ET_RS %>% select(-c("DOY","Year"))

ET_RS_melt <- ET_RS %>% melt(id.var = "Date")
names(ET_RS_melt) <- c("Date","Station","ET_RS")

ET_RS_melt <- ET_RS_melt %>% mutate(Key = paste(Date,Station,sep = "-"), .before = "Date")

### Creating Key for Wavelet Data
ET_BH <- ET_BH %>% mutate(Key = paste(DATE,Station,sep = "-"), .before = "DATE")

## Merging both df
ET_All <- merge(ET_BH, ET_RS_melt, by.x = "Key", by.y = "Key", all.x = TRUE)



#plotting
ggplot(data = ET_All, aes(x = ET, y =ET_RS, colour = Station.x)) + geom_point()

#Defining dates of HeatWave events
PreHeatWave1_Days <- c("2021-06-14","2021-06-15","2021-06-16")
HeatWave1_Days <- c("2021-06-17","2021-06-18","2021-06-19")
PostHeatWave1_Days <- c("2021-06-20","2021-06-21","2021-06-22")

PreHeatWave2_Days <- c("2021-07-06","2021-07-07","2021-07-08")
Heatwave2_Days <- c("2021-07-09","2021-07-10","2021-07-11")
PostHeatWave2_Days <- c("2021-07-12","2021-07-13","2021-07-14")

PreHeatWave3_Days <- c("2021-09-03","2021-09-04","2021-09-05")
Heatwave3_Days <- c("2021-09-06","2021-09-07","2021-09-08")
PostHeatWave3_Days <- c("2021-09-09","2021-09-10","2021-09-11")



#Adding HW events dates to the df
ET_All <- ET_All %>%
  mutate(HeatWave =  case_when( 
    Date %>% as.character() %in% PreHeatWave1_Days ~ "PreHW",
    Date %>% as.character() %in% HeatWave1_Days ~ "HW",
    Date %>% as.character() %in% PostHeatWave1_Days~ "PostHW",
    
    Date %>% as.character() %in% PreHeatWave2_Days ~ "PreHW",
    Date %>% as.character() %in% Heatwave2_Days ~ "HW",
    Date %>% as.character() %in% PostHeatWave2_Days~ "PostHW",
    
    Date %>% as.character() %in% PreHeatWave3_Days ~ "PreHW",
    Date %>% as.character() %in% Heatwave3_Days ~ "HW",
    Date %>% as.character() %in% PostHeatWave3_Days~ "PostHW",
    
    TRUE ~"NoHW"
    
  ))

#plotting

ET_All <- rename(ET_All, Station = Station.x)

lm(ET_RS~ET,data = ET_All %>% filter(HeatWave %in% c("PreHW","HW"))) %>% summary()

ggplot(data = ET_All %>% filter(HeatWave %in% c("PreHW","HW")), aes(x = ET, y =ET_RS)) +  geom_point() +
  #scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
  #                             "springgreen4","springgreen3","springgreen2",
  #                              "blue","royalblue4","skyblue3","purple")) +
  geom_smooth(method = "lm", se = TRUE) +
  xlim(c(1,7)) + ylim(c(1,7)) +
  xlab("Wavelet ET (mm/day)") + ylab("Remote Sensing ET (mm/day)") + ggtitle("Treatments are On") +
  geom_text(x = 2, y = 6,label = expression(italic("ET_RS = 2.99+0.24*ET; R2 = 0.27")),size = 3.0)

ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/BH_ET_analysis/ET_RS/Treatment_On_all_RS_regression.pdf")

