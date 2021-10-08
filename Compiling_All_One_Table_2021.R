rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(lubridate)
library(naniar)
library(stringr)
library(reshape2)

# Setting the directory
setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D18/') #Folder where you find the outputs from the compiling script "Compiling_Met_Thermo_Files.R" 
Dir <- getwd()


# Setting Output directory
Dir_Output <- c("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D18/Met_15min/")

#Get list of files in the Outcome Directory
Dat_Files <- list.files(Dir,pattern='[.]Rdata')

#Get only Met files
Dat_Met <- Dat_Files[grep("Met",Dat_Files)]

Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

Met_All <- NULL
for(i in 1:length(Stations_List)){
  
  Met_Station <- Dat_Met[grep(Stations_List[i],Dat_Met)]
  load(Met_Station)
  
  #Changing the name of the incoming file so it can run on a loop (dropping the station prefix)
  assign("Met", eval(parse(text = ls()[grep(Stations_List[i],ls())])))
  
  #TimeStamp as date-time class
  Met$TIMESTAMP <- as.POSIXct(Met$TIMESTAMP)
  
  #Dropping IRT_Max, and IRT Time Max
  Met <- Met %>% select(-RECORD)
  
  #Adding the Site  
  Met <- add_column(Met, Station = Stations_List[i], .after = "TIMESTAMP")    
  
  Met_All[[i]] <- Met
}
  

#Deleting Null elements from the list  
Met_All <- Met_All[!sapply(Met_All,is.null)]

#Consolidating the list
Met_All <- do.call(bind_rows,Met_All)

#Dropping columns that are trash
#Met_All <- Met_All %>% select(-Table_Met,-X15.MIN,-X1004,-X599164,-X63525,-Sec100Usec,-X........,-X,-X1982858864)

#save Rdat
setwd(Dir_Output)
#save(Met_All,file = "All_Met_15_min.Rdata")


Met_All <- add_column(Met_All, TimeStampSite = paste(Met_All$TIMESTAMP,Met_All$Station, sep = "-"),
                                       .before = "TIMESTAMP")

write.csv(Met_All,"Met_All_15min_2021.csv",row.names = FALSE)


########################################################################################
############################ Copying Net Radiation in all Stations ###################
###############################################################################################


#######Correcting for when the Net Radiometer was not leveled (17-21 June) #############
#(Copy pasting) the 15th of June in all the dates that are bugged

Date <- as.Date(Met_All$TIMESTAMP)

Rn_15Jun <- Met_All[Date == "2021-06-15" & Met_All$Station == "B2R2",c("Incoming_SW_Avg","Outgoing_SW_Avg","Incoming_LW_Avg","Outgoing_LW_Avg")]

Dates2change <- c("2021-06-17","2021-06-18","2021-06-19","2021-06-20","2021-06-21")

for(i in 1:length(Dates2change)){
Met_All[Date == Dates2change[i] & Met_All$Station == "B2R2",c("Incoming_SW_Avg","Outgoing_SW_Avg","Incoming_LW_Avg","Outgoing_LW_Avg")] <- Rn_15Jun
}

Met_B2R2_Radiation <- Met_All %>% filter(Station == "B2R2") %>% 
   select("TIMESTAMP","Incoming_SW_Avg",
          "Outgoing_SW_Avg","Incoming_LW_Avg","Outgoing_LW_Avg")


#Stations without Net Radiometer

Stations_List_NoRn <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R3",
                   "B3R1","B3R2","B3R3")

Met_TIMESTAMP <- NULL
Met_Station_Radiation <- NULL
for(i in 1:length(Stations_List_NoRn)){

   Met_TIMESTAMP[[i]] <- Met_All %>% filter(Station == Stations_List_NoRn[i]) %>% 
      select("TIMESTAMP")
   
   
   Met_Station_Radiation[[i]] <- merge(Met_TIMESTAMP[[i]], Met_B2R2_Radiation, by.x = "TIMESTAMP", by.y = "TIMESTAMP", all.x = TRUE)

   Met_All[Met_All$Station == Stations_List_NoRn[i],c('Incoming_SW_Avg','Outgoing_SW_Avg',
                                              'Incoming_LW_Avg','Outgoing_LW_Avg')] <- Met_Station_Radiation[[i]] %>% select('Incoming_SW_Avg','Outgoing_SW_Avg',                                                                                                                            'Incoming_LW_Avg','Outgoing_LW_Avg')
}


Met_All$Incoming_SW_Avg <- as.numeric(Met_All$Incoming_SW_Avg)
Met_All$Outgoing_SW_Avg <- as.numeric(Met_All$Outgoing_SW_Avg)
Met_All$Incoming_LW_Avg <- as.numeric(Met_All$Incoming_LW_Avg)
Met_All$Outgoing_LW_Avg <- as.numeric(Met_All$Outgoing_LW_Avg)

Met_All <- add_column(Met_All, R_net = (Met_All$Incoming_SW_Avg + Met_All$Incoming_SW_Avg) - (Met_All$Outgoing_SW_Avg + Met_All$Outgoing_LW_Avg), .after = "Outgoing_LW_Avg")

ggplot(data = Met_All, aes(x= TIMESTAMP, y =Incoming_SW_Avg, colour = Station)) + geom_line()

#save Rdat
setwd(Dir_Output)
save(Met_All,file = "All_Met_15_min.Rdata")



####################################################################
######################## Merging All The Tables ####################
####################################################################

#Load Thermocouples Table All
load("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Thermocouples_15min/All_Thermocouples_15_min.Rdata")

Thermocouples_All_15_min <- add_column(Thermocouples_All_15_min, TimeStampSite = paste(Thermocouples_All_15_min$TIMESTAMP,Thermocouples_All_15_min$Station, sep = "-"),
                                       .before = "TIMESTAMP")


#Load IRT Table All
load("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D5/IRT_15min/All_IRT_15_min.Rdata")

IRT_All_15_min <- add_column(IRT_All_15_min, TimeStampSite = paste(IRT_All_15_min$TIMESTAMP,IRT_All_15_min$Station, sep = "-"),
                                       .before = "TIMESTAMP")


   Met_Thermo_All <- merge(Met_All, IRT_All_15_min, by.x = "TimeStampSite", by.y = "TimeStampSite", all.x = TRUE)
   
   Met_Thermo_IRT_All_plus <- merge(Met_Thermo_All, Thermocouples_All_15_min, by.x = "TimeStampSite", by.y = "TimeStampSite", all.x = TRUE)
  
   Met_Thermo_IRT_All_plus <- arrange(Met_Thermo_IRT_All_plus,Station.x)
   
   Met_Thermo_IRT_All_plus <- Met_Thermo_IRT_All_plus %>% select(-TIMESTAMP,-TIMESTAMP.y,-Station,-Station.y) 
   
   Met_Thermo_IRT_All_plus <- Met_Thermo_IRT_All_plus %>% rename(Station = Station.x, TIMESTAMP = TIMESTAMP.x)
   
   write.csv(Met_Thermo_IRT_All_plus,"/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Met_15min/BordenHill2020_All.csv", row.names = FALSE)
   
   
   setwd(Dir_Output)
   write.csv(Met_Thermo_All,"BordenHill2021_All.csv", row.names = FALSE)
   
   
   
   
   
   
   
   Met_Thermo_All_select <- Met_Thermo_All %>% select(TIMESTAMP.x,Station.x,Temp_C_TypeE)
   Met_Thermo_All_select <- Met_Thermo_All_select %>% filter(Station.x =="B1R3" | Station.x =="B2R2" | Station.x =="B3R2")
   
   Met_Thermo_All_select <- Met_Thermo_All_select %>%  filter(Temp_C_TypeE < 50 ) # filtering for any weird value
   
   ggplot(data = Met_Thermo_All_select, aes(x=TIMESTAMP.x,y=Temp_C_TypeE, colour = Station.x)) + geom_line() +
     scale_x_datetime(limits = as.POSIXct(c("2021-04-12","2021-04-27"))) +labs(title = "Type E Thermocouples")+ xlab("Date") + ylab("Temp (C)")
   
   ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_D4_2021/TypeE.pdf")
   
   