rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(lubridate)
library(naniar)
library(stringr)
library(reshape2)
library(ggplot2)


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D16/Met_15min/")

Met_All_2021 <- read.csv("Met_All_15min_2021.csv")

#load("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D16/Met_15min/All_Met_15_min.Rdata")
#Met_All_2021 <- Met_All


Met_All_2021$TIMESTAMP <- ymd_hms(Met_All_2021$TIMESTAMP,tz=Sys.timezone())
Met_All_2021$TIMESTAMP <- as.POSIXct(Met_All_2021$TIMESTAMP)


Met_All_2021$Station <- as.factor(Met_All_2021$Station)


Met_All_2021$VW_Avg <- as.numeric(Met_All_2021$VW_Avg)
Met_All_2021$VW_Avg[Met_All_2021$VW_Avg == "NaN"] = NA

Met_All_2021$Soil_Temp_TCAV_Avg <- as.numeric(Met_All_2021$Soil_Temp_TCAV_Avg)
Met_All_2021$Soil_Temp_TCAV_Avg[Met_All_2021$Soil_Temp_TCAV_Avg == "NaN"] = NA
Met_All_2021$Soil_Temp_TCAV_Avg[Met_All_2021$Soil_Temp_TCAV_Avg == -7999.00] = NA


Met_All_2021$T_IRT_Avg <- as.numeric(Met_All_2021$T_IRT_Avg)
Met_All_2021$T_IRT_Avg[Met_All_2021$T_IRT_Avg == "NaN"] = NA
Met_All_2021$T_IRT_Avg[Met_All_2021$T_IRT_Avg == -7999.00] = NA



#Met_All_2021 <- add_column(Met_All_2021, Hour = lubridate::dmy_h(Met_All_2021$TIMESTAMP), .after = "TIMESTAMP")

#Met_All_2021 <- Met_All_2021 %>% filter(Station == "B3R1" | Station == "B3R2" | Station == "B3R3")


#Met_All_2021 <- Met_All_2021 %>% filter(Station == "B2R3")

  ggplot(data = Met_All_2021, aes(x = TIMESTAMP, y = T_IRT_Avg, color = Station))+geom_line()+ylim(0,50)+
    scale_x_datetime(limits = as.POSIXct(c("2021-07-01","2021-08-10"))) + labs(title = "IRT Temperature") +
    xlab("Days") + ylab("Temperature (C)")
  
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/ALL_IRT.pdf")
  
  
  Met_All_2021$AirTC_Avg <- as.numeric(Met_All_2021$AirTC_Avg)
  Met_All_2021$AirTC_Avg[Met_All_2021$Met_All_2021$AirTC_Avg == "NaN"] = NA
  
  
  Met_All_2021$RH <- as.numeric(Met_All_2021$RH)
  Met_All_2021$RH[Met_All_2021$RH == "NaN"] = NA
  #Met_All_2021$T_IRT_Avg[Met_All_2021$T_IRT_Avg == -7999.00] = NA
  
  
  Met_All_2021_Main <- Met_All_2021 %>% filter(Station == "B1R3" | Station == "B2R2" | Station =="B3R2")
  
  ggplot(data = Met_All_2021_Main, aes(x = TIMESTAMP, y = AirTC_Avg, color = Station))+geom_line()+ylim(0,50)+
    scale_x_datetime(limits = as.POSIXct(c("2021-06-14","2021-08-10"))) + labs(title = "Air Temperature") +
    xlab("Days") + ylab("Temperature (C)")
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/ALL_Air_Temperature.pdf")
  
  
  ggplot(data = Met_All_2021_Main, aes(x = TIMESTAMP, y = RH, color = Station))+geom_line()+ylim(0,100)+
    scale_x_datetime(limits = as.POSIXct(c("2021-07-01","2021-08-10"))) + labs(title = "Relative Humidity") +
    xlab("Days") + ylab("RH (%)")
  
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/ALL_RH.pdf")
  
  
  
###Soil Moisture#####   
  Met_All_2021$VW_Avg <- as.numeric(Met_All_2021$VW_Avg)
  Met_All_2021$VW_Avg[Met_All_2021$VW_Avg == "NaN"] <- NA

Met_All_2001_Bottom <- Met_All_2021 %>% filter(Station == "B3R3") %>% select(TIMESTAMP,Station,VW_Avg,VWC_Avg,VWC_2_Avg,VWC_3_Avg)
Met_All_2001_Bottom <- Met_All_2001_Bottom %>%  mutate(VW_Avg = VW_Avg*100)

Met_All_2001_Bottom <- Met_All_2001_Bottom %>% melt(id.vars = c("Station","TIMESTAMP"))


Met_All_2001_Bottom$value <- as.numeric(Met_All_2001_Bottom$value)
Met_All_2001_Bottom$value[Met_All_2001_Bottom$value == "NaN"] = NA
  
  ggplot(data = Met_All_2001_Bottom, aes(x = TIMESTAMP, y = value, color = variable)) + geom_line() +
    scale_color_discrete(name = "Depth (m)", labels = c("(CS) 0.05 m", "0.3 m","0.6 m", "0.9 m")) + ylim(0,60) +
    scale_x_datetime(limits = as.POSIXct(c("2021-06-01","2021-08-10")), date_breaks = "2 week", date_labels = "%d-%b") + 
    labs(title = "B3R3") + xlab("Day") + ylab("Soil moisture (%)") +
    annotate("rect", xmin =  as.POSIXct("2021-06-16"), xmax = as.POSIXct("2021-06-18") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.POSIXct("2021-06-17"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red')+
    
  annotate("rect", xmin =  as.POSIXct("2021-07-09"), xmax = as.POSIXct("2021-07-11") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
    annotate("text", x = as.POSIXct("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red')
  
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/B3R3_TDR.pdf")



  
###### Net Radiation ########  

Met_All_2021_Main_NetRad <- Met_All_2021 %>% filter(Station == "B2R2")
  
  Met_All_2021_Main_NetRad <- Met_All_2021_Main_NetRad %>% mutate(Net_Radiation = (Incoming_SW_Avg +Incoming_LW_Avg) - (Outgoing_SW_Avg + Outgoing_LW_Avg))
  

  Met_All_2021_Main_NetRad_merge <- Met_All_2021_Main_NetRad %>% select(TIMESTAMP,Incoming_SW_Avg,Incoming_LW_Avg,Outgoing_SW_Avg,Outgoing_LW_Avg, Net_Radiation)
  
  Met_All_2021_Main_NetRad_merge <- Met_All_2021_Main_NetRad_merge %>% melt(id.vars = "TIMESTAMP")
    
  ggplot(data = Met_All_2021_Main_NetRad_merge, aes(x = as.POSIXct(TIMESTAMP), y = value, color = variable)) + geom_line() +
    scale_color_discrete(name = "Radiation (W m-2)", labels = c("In SW", "In LW","Out SW", "Out Lw", "Net Radiation"))  +
    scale_x_datetime(limits = as.POSIXct(c("2021-07-20","2021-08-10")), date_breaks = "1 weeks", date_labels = "%b %d") + 
    labs(title = "Net Radiation") + xlab("Day") + ylab("Radiation (W m-2)") +
    annotate("rect", xmin =  as.POSIXct("2021-06-17"), xmax = as.POSIXct("2021-06-21") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "blue") +
    annotate("text", x = as.POSIXct("2021-06-19"), y = Inf, vjust = 1.16, label = "Net Rad not Leveled", size = 3, colour = 'black')
    
    
 
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/Net_RadiationS.pdf")
  
  
  
  ########### Wavelet Evapotranspiration ########
  
  ET_BH <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D16/Wavelet/ET_daily_BH_2021.csv")

  ET_BH$DATE <- as.Date(ET_BH$DATE)
  ET_BH$Station <- as.factor(ET_BH$Station)
  ET_BH$ET <- as.numeric(ET_BH$ET)
  ET_BH <- ET_BH %>% filter(ET > 0)

  
  
  ggplot(data = ET_BH, aes(x = DATE, y = ET, colour = Station)) + geom_line() +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3")) + 
    scale_x_date(limits = as.Date(c("2021-04-12","2021-08-10")),date_breaks = "2 week", date_labels = "%b %d")+
    ylab("ET (mm/day)") + xlab("Day")+labs(title = "Borden Hill Evapotranspiration")
  
  Reference_ET <- read.csv("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/daily_ETO_CIMIS_LEIDEN_0815.csv")
  Reference_ET$Date <- as.Date(Reference_ET$Date,"%m/%d/%Y")
  
  
  Reference_ET <- Reference_ET %>% select(Date, ETo..mm.)
  colnames(Reference_ET) <- c("DATE", "ET")
  Reference_ET <- add_column(Reference_ET, Station = "Reference ET", .after = "DATE")
  
  ET_BH <- ET_BH %>% select(DATE, Station, ET)
  ET_BH <- rbind(ET_BH,Reference_ET)
 
  
  ggplot(data = ET_BH, aes(x = DATE, y = ET, colour = Station)) + geom_line() +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3","purple")) + 
    scale_x_date(limits = as.Date(c("2021-04-15","2021-08-10")),date_breaks = "1 month", date_labels = "%b")+
    ylab("ET (mm/day)") + xlab("Day")+labs(title = "Borden Hill Evapotranspiration (2021)") +
    annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
    
    annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') +
  theme_classic()
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D16_2021/ET.jpg")
  
  
  ## Ploting Sensible Heat H
  
  Sensible_Heat <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D11/Wavelet/All_H_2021.csv")
  
  Sensible_Heat$H_WL[Sensible_Heat$H_WL > 1000] <- NA
  Sensible_Heat <- add_column(Sensible_Heat, Date = as.Date(Sensible_Heat$TIMESTAMP), .before = "TIMESTAMP")
  
  #Daily Sum H
  Daily_SH <-   Sensible_Heat %>% group_by(Date, Station) %>% summarise(Daily_H = sum(H_WL, na.rm = TRUE))
  
  #Changing from W m-2 to kJoule per day
  Daily_SH <- Daily_SH %>% mutate(Daily_H_kJDay = Daily_H*15*60/1000)
  
  ggplot(data = Daily_SH, aes(x = Date, y = Daily_H_kJDay, colour = Station)) + geom_line() +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3","purple")) + 
    scale_x_date(limits = as.Date(c("2021-04-15","2021-06-21")),date_breaks = "2 week", date_labels = "%b %d")+
    ylab("H (kJ/day)") + xlab("Day")+labs(title = "Borden Hill Sensible Heat (2021)") +
    annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-06-17"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red')
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D11_2021/H.pdf")
  
  
  ##Plotting G
  
  Soil_Heat_Flux <- Met_All %>% select(TIMESTAMP,Station,SHF_Avg)
  Soil_Heat_Flux <- add_column( Soil_Heat_Flux, Date = as.Date( Soil_Heat_Flux$TIMESTAMP), .before = "TIMESTAMP")
  Soil_Heat_Flux$SHF_Avg <- as.numeric(Soil_Heat_Flux$SHF_Avg)
  
  
  SHF_Daily <- Soil_Heat_Flux %>% group_by(Date,Station) %>% summarise(SHF_Daily = sum(SHF_Avg, na.rm = TRUE))
  #Changing from W m-2 to kJoule per day
  SHF_Daily <- SHF_Daily %>% mutate(Daily_SHF_kJDay = SHF_Daily*15*60/1000)
  
  ggplot(data = SHF_Daily, aes(x = Date, y = Daily_SHF_kJDay, colour = Station)) + geom_line() +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3","purple")) + 
    scale_x_date(limits = as.Date(c("2021-04-15","2021-06-21")),date_breaks = "2 week", date_labels = "%b %d")+
    ylab("G (kJ/day)") + xlab("Day")+labs(title = "Borden Hill SHF (2021)") +
    annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-06-17"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') 
    
   
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D11_2021/G.pdf")
  
  
  
  ##############################
  ######## TDR profiles ########
  ##############################
  
  Met_All_2021_TDR_select <- Met_All_2021 %>% select(TIMESTAMP,Station,VW_Avg,VWC_Avg,VWC_2_Avg,VWC_3_Avg)

  
  
  Met_All_2021_TDR_merge <- melt(Met_All_2021_TDR_select, id.vars = c("TIMESTAMP","Station"))
  
  Met_All_2021_TDR_depth <- Met_All_2021_TDR_merge %>% filter(variable == "VWC_3_Avg")
  
  
  ggplot(data = Met_All_2021_TDR_depth, aes(x= TIMESTAMP,y=value, colour = Station))+ geom_line() + ylim(0,70) +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3")) + scale_x_datetime(limits = as.POSIXct(c("2021-06-01","2021-07-21")),
                                  date_breaks = "1 week", date_labels = "%b-%d")+
    ylab("Soil Moisture (%)") + xlab("Day")+labs(title = "Accua (-0.90 m)") +
    annotate("rect", xmin =  as.POSIXct("2021-06-16"), xmax = as.POSIXct("2021-06-18") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
    annotate("text", x = as.POSIXct("2021-06-17"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
    
    annotate("rect", xmin =  as.POSIXct("2021-07-09"), xmax = as.POSIXct("2021-07-11") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.POSIXct("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red')
  
  
  
  ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D14_2021/Accua_090m.pdf")
  
  
  
  
  
  
  names(Met_All_2021)
  
  ggplot(Met_All_2021, aes(x= TIMESTAMP, y =Soil_Temp_Avg, colour = Station))+geom_line() + 
    scale_color_manual(values  = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
                                  "blue","royalblue4","skyblue3")) +
    scale_x_datetime(limits = as.POSIXct(c("2021-04-04","2021-04-08")),date_breaks = "1 day", date_labels = "%b %d") +ylim(15,25)
  
  
  
  
  
  
