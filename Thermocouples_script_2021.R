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
setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D12/') #Folder where you find the outputs from the compiling script "Compiling_Met_Thermo_Files.R" 
Dir <- getwd()


# Setting Output directory
Dir_Output <- c("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D12/Met_15min/")

#Get list of files in the Outcome Directory
Dat_Files <- list.files(Dir,pattern='[.]Rdata')


#Get only Thermocouples files
Dat_TC <- Dat_Files[grep("Thermocouples",Dat_Files)]

Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

ThermoCouples_All <- NULL

for(i in 1:length(Stations_List)){
  
  TC_Station <- Dat_TC[grep(Stations_List[i],Dat_TC)]
  load(TC_Station)
  
  #Changing the name of the incoming file so it can run on a loop (dropping the station prefix)
  assign("ThermoCouple", eval(parse(text = ls()[grep(Stations_List[i],ls())])))
  
  #TimeStamp as date-time class
  ThermoCouple$TIMESTAMP <- as.POSIXct(ThermoCouple$TIMESTAMP)
  
  #Dropping IRT_Max, and IRT Time Max
  ThermoCouple <- ThermoCouple %>% select(-RECORD)
  

   #Creating Tags for the 20 channels of thermocouples
  n = 20
  ThermoCouples_names <- matrix(nrow = 1, ncol = n, data = NA)
  for(j in 1:n){
    ThermoCouples_names[1,j] <- paste("Therm_Couple",as.character(j),sep = "_")  
  }
  
  
  if( i == 5){
  #Selecting only useful columns (TimeStamp + the 20 TC channels)
  ThermoCouple_selection <- ThermoCouple %>% select(TIMESTAMP,starts_with("Temp_C."))
  
  }else{ThermoCouple_selection <- ThermoCouple %>% select(TIMESTAMP,starts_with("Temp_C_Avg."))
  }
  for(j in 2:21){
    
    ThermoCouple_selection[,j] <- as.numeric( ThermoCouple_selection[,j])
    ThermoCouple_selection[,j][ThermoCouple_selection[,j] == "NaN"]  <- NA
  }
  
  #Naming columns
  colnames(ThermoCouple_selection) <- c("TIMESTAMP",as.vector(ThermoCouples_names))
  
  #adding a time ceiling to have time records every 15 minutes
  ThermoCouple_selection <- ThermoCouple_selection %>% add_column(Ceiling_date = ceiling_date(ThermoCouple_selection$TIMESTAMP, "15 mins"),.after = "TIMESTAMP")
  
  #Averaging temperatures based on ceiling timestamp
  ThermoCouple_Ave <- ThermoCouple_selection %>% group_by(Ceiling_date) %>% summarise(across(where(is.numeric),mean, na.rm = TRUE))
  
  #Changing name from celing to TIMESTAMP
  ThermoCouple_Ave <- ThermoCouple_Ave %>% rename(TIMESTAMP = Ceiling_date)
  
  #Adding the Site  
  ThermoCouple_Ave <- add_column(ThermoCouple_Ave, Station = Stations_List[i], .after = "TIMESTAMP")    
  
  
  ThermoCouples_All[[i]] <- ThermoCouple_Ave
}

ThermoCouples_Final <- do.call(bind_rows, ThermoCouples_All)

#####################  
####### B1R1 #######
####### ####### ####### 


B1R1 <-read.csv("19170 (B1R1)_Thermocouples_out.csv")


#Creating Tags for the 20 channels of thermocouples
n = 20
ThermoCouples_names <- matrix(nrow = 1, ncol = n, data = NA)
for(i in 1:n){
  ThermoCouples_names[1,i] <- paste("Therm_Couple",as.character(i),sep = "_")  
}

#Naming columns
colnames(B1R1) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B1R1$TimeStamp <- as.POSIXct(B1R1$TimeStamp)

#ThermoCouples as numeric
B1R1[,grep("Therm_Couple",colnames(B1R1))] <- lapply(B1R1[,grep("Therm_Couple",colnames(B1R1))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B1R1[,i] <- as.numeric( B1R1[,i])
  B1R1[,i][B1R1[,i] == "NaN"]  <- NA
}


#Selecting only the 20 Thermo Couple channels 
B1R1_selection <- B1R1 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B1R1_selection <- B1R1_selection %>% add_column(Floor_date = floor_date(B1R1_selection$TimeStamp, "15 mins"),.after = "TimeStamp")

#hola <- aggregate(.~Floor_date, B1R1_selection, mean, na.action = na.omit)


Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B1R1_selection[,i+2]~B1R1_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B1R1_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B1R1_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)


#ReNaming columns
colnames(B1R1_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))

#Adding the Site  
B1R1_15min_Ave <- add_column(B1R1_15min_Ave, Tag = "B1R1", .after = "TimeStamp")    
    
    


#####################  
####### B1R4 #######
####### ####### ####### 


B1R4 <-read.csv("18331 (B1R4)_Thermocouples_out.csv")

#Naming columns
colnames(B1R4) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B1R4$TimeStamp <- as.POSIXct(B1R4$TimeStamp)

#ThermoCouples as numeric
B1R4[,grep("Therm_Couple",colnames(B1R4))] <- lapply(B1R4[,grep("Therm_Couple",colnames(B1R4))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B1R4[,i] <- as.numeric( B1R4[,i])
  B1R4[,i][B1R4[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B1R4_selection <- B1R4 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B1R4_selection <- B1R4_selection %>% add_column(Floor_date = floor_date(B1R4_selection$TimeStamp, "15 mins"),.after = "TimeStamp")


Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B1R4_selection[,i+2]~B1R4_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B1R4_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B1R4_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)



#ReNaming columns
colnames(B1R4_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B1R4_15min_Ave <- add_column(B1R4_15min_Ave, Tag = "B1R4", .after = "TimeStamp")    



#####################  
####### B1R3 #######
####### ####### ####### 

B1R3 <-read.csv("17702 (B1R3)_Thermocouples_out.csv")


#Naming columns
colnames(B1R3) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B1R3$TimeStamp <- as.POSIXct(B1R3$TimeStamp)

#ThermoCouples as numeric
B1R3[,grep("Therm_Couple",colnames(B1R3))] <- lapply(B1R3[,grep("Therm_Couple",colnames(B1R3))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B1R3[,i] <- as.numeric( B1R3[,i])
  B1R3[,i][B1R3[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B1R3_selection <- B1R3 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B1R3_selection <- B1R3_selection %>% add_column(Floor_date = floor_date(B1R3_selection$TimeStamp, "15 mins"),.after = "TimeStamp")



Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B1R3_selection[,i+2]~B1R3_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B1R3_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B1R3_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)



#ReNaming columns
colnames(B1R3_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B1R3_15min_Ave <- add_column(B1R3_15min_Ave, Tag = "B1R3", .after = "TimeStamp")  




#####################  
####### B2R1 #######
####### ####### ####### 


B2R1 <-read.csv("19120 (B2R1)_Thermocouples_out.csv")


#Naming columns
colnames(B2R1) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B2R1$TimeStamp <- as.POSIXct(B2R1$TimeStamp)

#ThermoCouples as numeric
B2R1[,grep("Therm_Couple",colnames(B2R1))] <- lapply(B2R1[,grep("Therm_Couple",colnames(B2R1))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B2R1[,i] <- as.numeric( B2R1[,i])
  B2R1[,i][B2R1[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B2R1_selection <- B2R1 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B2R1_selection <- B2R1_selection %>% add_column(Floor_date = floor_date(B2R1_selection$TimeStamp, "15 mins"),.after = "TimeStamp")



Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B2R1_selection[,i+2]~B2R1_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B2R1_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B2R1_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)


#ReNaming columns
colnames(B2R1_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B2R1_15min_Ave <- add_column(B2R1_15min_Ave, Tag = "B2R1", .after = "TimeStamp")  



#####################  
####### B2R2 #######
####### ####### ####### 


B2R2 <-read.csv("18333 (B2R2)_Thermocouples_out.csv")

#Naming columns
colnames(B2R2) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B2R2$TimeStamp <- as.POSIXct(B2R2$TimeStamp)

#ThermoCouples as numeric
B2R2[,grep("Therm_Couple",colnames(B2R2))] <- lapply(B2R2[,grep("Therm_Couple",colnames(B2R2))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B2R2[,i] <- as.numeric( B2R2[,i])
  B2R2[,i][B2R2[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B2R2_selection <- B2R2 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B2R2_selection <- B2R2_selection %>% add_column(Floor_date = floor_date(B2R2_selection$TimeStamp, "15 mins"),.after = "TimeStamp")




Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B2R2_selection[,i+2]~B2R2_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B2R2_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B2R2_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)


#ReNaming columns
colnames(B2R2_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B2R2_15min_Ave <- add_column(B2R2_15min_Ave, Tag = "B2R2", .after = "TimeStamp")  




#####################  
####### B2R3 #######
####### ####### ####### 


B2R3 <-read.csv("3859 (B2R3)_Thermocouples_out.csv")

#Naming columns
colnames(B2R3) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B2R3$TimeStamp <- as.POSIXct(B2R3$TimeStamp)

#ThermoCouples as numeric
B2R3[,grep("Therm_Couple",colnames(B2R3))] <- lapply(B2R3[,grep("Therm_Couple",colnames(B2R3))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B2R3[,i] <- as.numeric( B2R3[,i])
  B2R3[,i][B2R3[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B2R3_selection <- B2R3 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B2R3_selection <- B2R3_selection %>% add_column(Floor_date = floor_date(B2R3_selection$TimeStamp, "15 mins"),.after = "TimeStamp")




Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B2R3_selection[,i+2]~B2R3_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B2R3_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B2R3_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)




#ReNaming columns
colnames(B2R3_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B2R3_15min_Ave <- add_column(B2R3_15min_Ave, Tag = "B2R3", .after = "TimeStamp")  




#####################  
####### B3R1 #######
####### ####### ####### 


B3R1 <-read.csv("18330 (B3R1)_Thermocouples_out.csv")



#Naming columns
colnames(B3R1) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B3R1$TimeStamp <- as.POSIXct(B3R1$TimeStamp)

#ThermoCouples as numeric
B3R1[,grep("Therm_Couple",colnames(B3R1))] <- lapply(B3R1[,grep("Therm_Couple",colnames(B3R1))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B3R1[,i] <- as.numeric( B3R1[,i])
  B3R1[,i][B3R1[,i] == "NaN"]  <- NA
}

#Selecting only the 20 Thermo Couple channels 
B3R1_selection <- B3R1 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B3R1_selection <- B3R1_selection %>% add_column(Floor_date = floor_date(B3R1_selection$TimeStamp, "15 mins"),.after = "TimeStamp")




Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B3R1_selection[,i+2]~B3R1_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B3R1_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B3R1_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)



#ReNaming columns
colnames(B3R1_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B3R1_15min_Ave <- add_column(B3R1_15min_Ave, Tag = "B3R1", .after = "TimeStamp")  



#####################  
####### B3R2 #######
####### ####### ####### 


B3R2 <-read.csv("18335 (B3R2)_Thermocouples_out.csv")


#Naming columns
colnames(B3R2) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B3R2$TimeStamp <- as.POSIXct(B3R2$TimeStamp)

#ThermoCouples as numeric
B3R2[,grep("Therm_Couple",colnames(B3R2))] <- lapply(B3R2[,grep("Therm_Couple",colnames(B3R2))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B3R2[,i] <- as.numeric( B3R2[,i])
  B3R2[,i][B3R2[,i] == "NaN"]  <- NA
}


#Selecting only the 20 Thermo Couple channels 
B3R2_selection <- B3R2 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B3R2_selection <- B3R2_selection %>% add_column(Floor_date = floor_date(B3R2_selection$TimeStamp, "15 mins"),.after = "TimeStamp")



Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B3R2_selection[,i+2]~B3R2_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B3R2_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B3R2_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)


#ReNaming columns
colnames(B3R2_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B3R2_15min_Ave <- add_column(B3R2_15min_Ave, Tag = "B3R2", .after = "TimeStamp")  



#####################  
####### B3R3 #######
####### ####### ####### 


B3R3 <-read.csv("19171 (B3R3)_Thermocouples_out.csv")

#Naming columns
colnames(B3R3) <- c("TimeStamp","Record","Temp_Ave",as.vector(ThermoCouples_names))


#TimeStamp as date-time class
B3R3$TimeStamp <- as.POSIXct(B3R3$TimeStamp)

#ThermoCouples as numeric
B3R3[,grep("Therm_Couple",colnames(B3R3))] <- lapply(B3R3[,grep("Therm_Couple",colnames(B3R3))],as.numeric)

#Making ThermoCouples channels numeric class and changing NaNs for NA
for(i in 3:23){
  
  B3R3[,i] <- as.numeric( B3R3[,i])
  B3R3[,i][B3R3[,i] == "NaN"]  <- NA
}


#Selecting only the 20 Thermo Couple channels 
B3R3_selection <- B3R3 %>% select(TimeStamp,starts_with("Therm_Couple"))

#adding a floor to have time records every 15 minutes
B3R3_selection <- B3R3_selection %>% add_column(Floor_date = floor_date(B3R3_selection$TimeStamp, "15 mins"),.after = "TimeStamp")



Thermocouple_Channel <- list()
for (i in 1:n){
  
  Thermocouple_Channel[[i]] <- (aggregate(B3R3_selection[,i+2]~B3R3_selection$Floor_date,FUN=mean,na.action = na.omit))
}


B3R3_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "B3R3_selection$Floor_date",all = TRUE),
                         Thermocouple_Channel)


#ReNaming columns
colnames(B3R3_15min_Ave) <- c("TimeStamp",as.vector(ThermoCouples_names))


#Adding the Site  
B3R3_15min_Ave <- add_column(B3R3_15min_Ave, Tag = "B3R3", .after = "TimeStamp")  

###########################################
###########################################

#Compiling all different stations in one single dataframe 

ThermoCouples_All <- bind_rows(B1R1_15min_Ave,B1R4_15min_Ave,B1R3_15min_Ave,
                               B2R1_15min_Ave,B2R2_15min_Ave,B2R3_15min_Ave,
                               B3R1_15min_Ave,B3R2_15min_Ave,B3R3_15min_Ave)



#Filtering for weird values (below 0 C and above 75 C)
ThermoCouples_All[, grep("Therm",names(ThermoCouples_All))][ThermoCouples_All[, grep("Therm",names(ThermoCouples_All))] <= 0] <- NA
ThermoCouples_All[, grep("Therm",names(ThermoCouples_All))][ThermoCouples_All[, grep("Therm",names(ThermoCouples_All))] >= 75] <- NA



#######Adding Meteorological data for each station########


Met_data <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Met_All_stations_out.csv')

Met_data <- Met_data %>% 
  mutate(Station = case_when( 
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "17702 (B1R3)"  ~ "B1R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18331 (B1R4)" ~ "B1R4",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "19171 (B3R3)" ~ "B3R3", 
    Station == "19170 (B1R1)" ~ "B1R1",
    
    TRUE ~ as.character(Station)
  ))



#Generating a TimeStamp + Site tag to merge Thermocouples with Met Data dataframes

ThermoCouples_All <- add_column(ThermoCouples_All, TimeSite = paste(ThermoCouples_All$TimeStamp,ThermoCouples_All$Tag,sep = "-"), .before = "TimeStamp")

Met_data <- add_column(Met_data, TimeSite = paste(Met_data$TIMESTAMP,Met_data$Station,sep = "-"), .before = "TIMESTAMP")


Met_Thermo_All <- merge(Met_data, ThermoCouples_All, by.x = "TimeSite", by.y = "TimeSite", all.x = TRUE)


##################################
####### Testing Stuff ############
##################################

B1R4_TC <- ThermoCouples_All %>% filter(Tag == "B1R4")
B1R4_MSt <- Met_data %>% filter(Station == "B1R4")

B1R4_All <- Reduce(function(x,y) merge(x = x, y = y, by = "TimeSite"),
                         list(B1R4_TC,B1R4_MSt))


B1R4_MSt_Days <- unique(paste(lubridate::month(B1R4_MSt$TIMESTAMP),lubridate::day(B1R4_MSt$TIMESTAMP),sep = '-'))

B1R4_TC_Days <- unique(paste(lubridate::month(B1R4_TC$TimeStamp),lubridate::day(B1R4_TC$TimeStamp),sep = '-'))


Date_All <- as.data.frame(NULL)
for(i in 1:nrow(B1R4_All)){

  pos_Site <- gregexpr("-",B1R4_All$TimeSite[i])[[1]][length(gregexpr("-",B1R4_All$TimeSite[i])[[1]])]

Date_All[i,1] <- as.POSIXct(str_sub(B1R4_All$TimeSite[i],start = 1, end = pos_Site-1))
}
class(Date_All)


B1R4_All_Days <- unique(paste(lubridate::month(Date_All[,1]),lubridate::day(Date_All[,1]),sep = '-'))
######################################################################################################


##Selecting most important variables
Met_Thermo_All_Selection <- Met_Thermo_All %>% select(TimeSite,PA_uS_Avg,Soil_Temp_TCAV_Avg,
                                                      SHF_Avg,T_IRT_Avg,AirTC_Avg,RH,VW_Avg,VWC_Avg,Soil_Temp_Avg,
                                                      S_EC_Avg,SPW_EC_Avg,AirTC_AC_Avg,RH_AC,
                                                      Incoming_SW_Avg,Outgoing_SW_Avg,Incoming_LW_Avg,Outgoing_LW_Avg,
                                                      as.vector(ThermoCouples_names))


####The last 2 Rows are being naughty, I will look into that later
#delete last 2 rows

Met_Thermo_All_Selection <- Met_Thermo_All_Selection[-c(nrow(Met_Thermo_All_Selection)-1,nrow(Met_Thermo_All_Selection)),] 


### Adding TimeStamp and Station, Block, and Rep, in different columns
Date_All <- as.data.frame(NULL)
Site_All <- matrix(data = NA, nrow = nrow(Met_Thermo_All_Selection),ncol = 1)
Block_All <- matrix(data = NA, nrow = nrow(Met_Thermo_All_Selection),ncol = 1)
Rep_All <- matrix(data = NA, nrow = nrow(Met_Thermo_All_Selection),ncol = 1)

for(i in 1:nrow(Met_Thermo_All_Selection)){
  
  pos_Site <- gregexpr("-",Met_Thermo_All_Selection$TimeSite[i])[[1]][length(gregexpr("-",Met_Thermo_All_Selection$TimeSite[i])[[1]])]
  
  Date_All[i,1] <- as.POSIXct(str_sub(Met_Thermo_All_Selection$TimeSite[i],start = 1, end = pos_Site-1))
  Site_All[i,1] <- str_sub(Met_Thermo_All_Selection$TimeSite[i],start = pos_Site+1, end = nchar(Met_Thermo_All_Selection$TimeSite[i]))
  Block_All[i,1] <- str_sub(Site_All[i,1], start = 1, end = 2)
  Rep_All[i,1] <- str_sub(Site_All[i,1], start = 3, end = 4)
  }

class(Date_All)

#Adding the columns we created
Met_Thermo_All_Selection <- add_column(Met_Thermo_All_Selection,TimeStamp = Date_All$V1, .before = "TimeSite")
Met_Thermo_All_Selection <- add_column(Met_Thermo_All_Selection,Station = Site_All, .after = "TimeStamp")
Met_Thermo_All_Selection <- add_column(Met_Thermo_All_Selection,Block = Block_All, .after = "Station")
Met_Thermo_All_Selection <- add_column(Met_Thermo_All_Selection,Rep = Rep_All, .after = "Block") 


#Sorting by Station to make it tidy
Met_Thermo_All_Selection <- Met_Thermo_All_Selection %>% arrange(Station)

#Converting numeric columns to numeric
Met_Thermo_All_Selection[,seq(1,ncol(Met_Thermo_All_Selection),1)] <- as.numeric(Met_Thermo_All_Selection[,seq(1,ncol(Met_Thermo_All_Selection),1)])
lapply(Met_Thermo_All_Selection[,seq(1,ncol(Met_Thermo_All_Selection),1)], class)


#Plotting some stuff
Met_Thermo_All_Selection$AirTC_AC_Avg <- as.numeric(Met_Thermo_All_Selection$AirTC_AC_Avg)
Met_Thermo_All_Selection$AirTC_AC_Avg[Met_Thermo_All_Selection$AirTC_AC_Avg == "NaN"] <- NA

Met_Thermo_All_Selection$AirTC_Avg <- as.numeric(Met_Thermo_All_Selection$AirTC_Avg)
Met_Thermo_All_Selection$AirTC_Avg[Met_Thermo_All_Selection$AirTC_Avg == "NaN"] <- NA
Met_Thermo_All_Selection$AirTC_Avg[Met_Thermo_All_Selection$AirTC_Avg <= 5] <- NA


Met_Thermo_All_Selection$RH <- as.character(Met_Thermo_All_Selection$RH) %>% as.numeric
Met_Thermo_All_Selection$RH[Met_Thermo_All_Selection$RH == "NaN"] <- NA
Met_Thermo_All_Selection$AirTC_AC_Avg <- as.numeric(Met_Thermo_All_Selection$AirTC_AC_Avg)


Met_Thermo_All_Selection$VWC_Avg <- as.character(Met_Thermo_All_Selection$VWC_Avg) %>% as.numeric
Met_Thermo_All_Selection$VWC_Avg[Met_Thermo_All_Selection$VWC_Avg == "NaN"] <- NA
Met_Thermo_All_Selection$VWC_Avg <- as.numeric(Met_Thermo_All_Selection$VWC_Avg)

Met_Thermo_All_Selection$VW_Avg <- as.character(Met_Thermo_All_Selection$VW_Avg) %>% as.numeric
Met_Thermo_All_Selection$VW_Avg[Met_Thermo_All_Selection$VW_Avg == "NaN"] <- NA
Met_Thermo_All_Selection$VW_Avg <- as.numeric(Met_Thermo_All_Selection$VW_Avg)




which(Met_Thermo_All_Selection$AirTC_AC_Avg == max(Met_Thermo_All_Selection$AirTC_AC_Avg,na.rm = TRUE))


day_month <- paste(lubridate::day(Met_Thermo_All_Selection$TimeStamp),lubridate::month(Met_Thermo_All_Selection$TimeStamp),sep='-')

hottest_day <- which(day_month == "16-8")

Hottest_Day <- Met_Thermo_All_Selection[hottest_day,]

      
p <- ggplot(Hottest_Day, aes(x=TimeStamp, y=T_IRT_Avg,color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Canopy IRT (°C)")

pdf("./Plots/T_IRT.pdf")
print(p)     # Plot 1 --> in the first page of PDF
dev.off()

l <-  ggplot(Hottest_Day, aes(x=TimeStamp, y=RH,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Relative Humidity (%)")

m <- ggplot(Hottest_Day, aes(x=TimeStamp, y=AirTC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Air Temp in Canopy (°C)")


n <- ggplot(Hottest_Day, aes(x=TimeStamp, y=AirTC_AC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Air Temp in Above-Canopy (°C)")



vwc <- ggplot(Hottest_Day, aes(x=TimeStamp, y=VWC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Soil Vol. Water Content (%)")





vw <- ggplot(Hottest_Day, aes(x=TimeStamp, y=VW_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020) (the other sensor)")+
  labs(x="Time",y="Soil Vol. Water Content (n/u)")

pdf("./Plots/VW_BSensor.pdf")
print(vw)     # Plot 1 --> in the first page of PDF
dev.off()



Hottest_Day_MainSts <- Hottest_Day %>% filter(Station == "B1R3"|Station == "B3R2"|Station == "B2R2")




l <-  ggplot(Hottest_Day_MainSts, aes(x=TimeStamp, y=RH,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Relative Humidity (%)")

pdf("./Plots/Relative_Humidity.pdf")
print(l)     # Plot 1 --> in the first page of PDF
dev.off()




m <- ggplot(Hottest_Day_MainSts, aes(x=TimeStamp, y=AirTC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Air Temp in Canopy (°C)")

pdf("./Plots/Air_Temp_Canopy.pdf")
print(m)     # Plot 1 --> in the first page of PDF
dev.off()



n <- ggplot(Hottest_Day_MainSts, aes(x=TimeStamp, y=AirTC_AC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Air Temp in Above-Canopy (°C)")

pdf("./Plots/Air_Temp_Above_Canopy.pdf")
print(n)     # Plot 1 --> in the first page of PDF
dev.off()



vwc <- ggplot(Hottest_Day_MainSts, aes(x=TimeStamp, y=VWC_Avg,  color=Station)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Soil Vol. Water Content (%)")


pdf("./Plots/WCC_Asensor.pdf")
print(vwc)     # Plot 1 --> in the first page of PDF
dev.off()



write.csv(Met_Thermo_All_Selection,"./Outputs/Met_Thermo_All_Selection.csv",row.names = FALSE)



Hottest_Day_Selection <- Hottest_Day_Selection %>% select(TimeStamp,Station,AirTC_Avg,AirTC_AC_Avg)

test_data_long <- melt(Hottest_Day_Selection, id=c("TimeStamp","Station"))  # convert to long format

Bambach <- test_data_long %>% filter(variable == "AirTC_Avg"|variable == "AirTC_AC_Avg")

Bambach <- add_column(Bambach, nico = paste(Bambach$Station,Bambach$variable,sep = "-"),.after = "variable")

ggplot(Bambach, aes(x=TimeStamp, y=value,  color=nico)) +
  geom_line() + ggtitle("Hottest Day (16-Aug-2020)")+
  labs(x="Time",y="Air Temp (°C)")


pdf("Tmax_TimeSeries.pdf")
print(p)     # Plot 1 --> in the first page of PDF
dev.off()


#### Beth Figures

Met_Thermo_All_Selection_16 <- Met_Thermo_All_Selection %>% filter(lubridate::hour(Met_Thermo_All_Selection$TimeSite) == 16)
Met_Thermo_All_Selection_16 <- Met_Thermo_All_Selection_16 %>% select(Block,as.vector(ThermoCouples_names))  

invento <- melt(Met_Thermo_All_Selection_16,id="Block")


beth <- ggplot(invento, aes(x = value, fill = Block)) +
  geom_density(alpha = .3) + labs(x = "Temp (°C)")
pdf("ThermoCouples_dis.pdf")
print(beth)     # Plot 1 --> in the first page of PDF
dev.off()

#Saving csv files

write.csv(B1R1_15min_Ave,"./Outputs/B1R1.csv",row.names = FALSE)
write.csv(B1R4_15min_Ave,"./Outputs/B1R4.csv",row.names = FALSE)
write.csv(B1R3_15min_Ave,"./Outputs/B1R3.csv",row.names = FALSE)

write.csv(B2R1_15min_Ave,"./Outputs/B2R1.csv",row.names = FALSE)
write.csv(B2R2_15min_Ave,"./Outputs/B2R2.csv",row.names = FALSE)
write.csv(B2R3_15min_Ave,"./Outputs/B2R3.csv",row.names = FALSE)

write.csv(B3R1_15min_Ave,"./Outputs/B3R1.csv",row.names = FALSE)
write.csv(B3R2_15min_Ave,"./Outputs/B3R2.csv",row.names = FALSE)
write.csv(B3R3_15min_Ave,"./Outputs/B3R3.csv",row.names = FALSE)





Decimal_Dates <- lubridate::decimal_date(B3R3_15min_Ave$TimeStamp) %>% sort
Dates <- paste(lubridate::month(date_decimal(Decimal_Dates)),lubridate::day(date_decimal(Decimal_Dates)),sep = "-") %>% unique
Dates
