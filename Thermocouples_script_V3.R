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


# Setting the directory
setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/Outputs_2021/D18/') #Folder where you find the outputs from the compiling script "Compiling_Met_Thermo_Files.R" 
Dir <- getwd()

# Setting Output directory
Dir_Output <- c("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/Outputs_2021/D18/Thermocouples_15min/")

#Get list of files in the Outcome Directory
Dat_Files <- list.files(Dir,pattern='[.]Rdata')

#Get only Thermocouples files
Dat_Thermocouples <- Dat_Files[grep("Thermocouples",Dat_Files)]

Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

Thermocouples_All_15_min <- NULL
for(i in 1:length(Stations_List)){
  
  Thermocouples_Station <- Dat_Thermocouples[grep(Stations_List[i],Dat_Thermocouples)]
  load(Thermocouples_Station)
  
  #Changing the name of the incoming file so it can run on a loop (dropping the station prefix)
  assign("Thermocouples", eval(parse(text = ls()[grep(Stations_List[i],ls())])))
  
  #20 Thermocouples channels
  n = 20
  ThermoCouples_names <- matrix(nrow = 1, ncol = n, data = NA)
  for(j in 1:n){
    ThermoCouples_names[1,j] <- paste("Therm_Couple",as.character(j),sep = "_")  
  }
  
  #Naming columns
  colnames(Thermocouples)[c(1,2,3)] <- c("TIMESTAMP","RECORD","Therm_Couple_Ave")
  colnames(Thermocouples)[grep("Temp_C_Avg",names(Thermocouples))] <- as.vector(ThermoCouples_names)
  
  #Selecting most important columns
  Thermocouples_Selection <- Thermocouples %>% select(TIMESTAMP,Therm_Couple_Ave,starts_with("Therm_Couple"))
  
  #TimeStamp as date-time class
  Thermocouples_Selection$TIMESTAMP <- as.POSIXct(Thermocouples_Selection$TIMESTAMP)
  
  #ThermoCouples as numeric
  Thermocouples_Selection[,grep("Therm_Couple",colnames(Thermocouples_Selection))] <- lapply(Thermocouples_Selection[,grep("Therm_Couple",colnames(Thermocouples_Selection))],as.numeric)
  
  
  only_Therm_channels <- grep("Therm_Couple", names(Thermocouples_Selection))
  #Changing NaNs for NA and filtering weird values
  for(j in 1:length(only_Therm_channels)){
    Thermocouples_Selection[,only_Therm_channels[j]] <- as.numeric(Thermocouples_Selection[,only_Therm_channels[j]])
    Thermocouples_Selection[,only_Therm_channels[j]][Thermocouples_Selection[,only_Therm_channels[j]] == "NaN"]  <- NA
    Thermocouples_Selection[,only_Therm_channels[j]][Thermocouples_Selection[,only_Therm_channels[j]] < -5]  <- NA
    Thermocouples_Selection[,only_Therm_channels[j]][Thermocouples_Selection[,only_Therm_channels[j]] > 70]  <- NA
  }
  
  
  #adding a ceiling to have time records every 15 minutes
  Thermocouples_Selection <- Thermocouples_Selection %>% add_column(Floor_date = ceiling_date(Thermocouples_Selection$TIMESTAMP, "15 mins"),.after = "TIMESTAMP")
  
  only_Therm_channels <- grep("Therm_Couple", names(Thermocouples_Selection))
  
  #Averaging at a 15 minute interval
  Thermocouple_Channel <- list()
    for (j in 1:length(only_Therm_channels)){
      Thermocouple_Channel[[j]] <- (aggregate(Thermocouples_Selection[,only_Therm_channels[j]]~Thermocouples_Selection$Floor_date,FUN=mean,na.action = na.omit))
    }
  
  Thermocouples_15min_Ave <- Reduce(function(x,y) merge(x = x, y = y, by = "Thermocouples_Selection$Floor_date",all = TRUE),
                           Thermocouple_Channel)
  
  
  #ReNaming columns
  colnames(Thermocouples_15min_Ave) <- c("TIMESTAMP","Therm_Couple_Ave",as.vector(ThermoCouples_names))
  
  #Adding the Site  
  Thermocouples_15min_Ave <- add_column(Thermocouples_15min_Ave, Station = Stations_List[i], .after = "TIMESTAMP")    
  
  #Write csv
  #write.csv(Thermocouples_15min_Ave, file = paste0(Dir_Output,paste(Stations_List[i],"Thermocouples.csv",sep = "_")),row.names = FALSE)
  
  #Saving All Stations in one list
  Thermocouples_All_15_min[[i]] <- Thermocouples_15min_Ave
  
  }

#Consolidating the list
#Thermocouples_All_15_min <- do.call(bind_rows,Thermocouples_All_15_min)

#save Rdat
#setwd(Dir_Output)
#save(Thermocouples_All_15_min,file = "All_Thermocouples_15_min.Rdata")



#CLEANING PROCESS
###########################################
###########################################
#B1R1
###########################################
###########################################
Thermocouples_15min_B1R1 <- Thermocouples_All_15_min[[1]]
Thermocouples_15min_B1R1_Only_Thermocouples <- Thermocouples_15min_B1R1 %>% select_if(is.numeric)


Mean_rows <- rowMeans(Thermocouples_15min_B1R1_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B1R1_Only_Thermocouples,1,sd, na.rm = TRUE)

Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B1R1_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B1R1_Only_Thermocouples))


cut <- 2
for(i in 1:nrow(Thermocouples_15min_B1R1_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B1R1_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                    else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                    else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                    else{x = x})
  
}

Thermocouples_15min_B1R1 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B1R1) <- names(Thermocouples_15min_B1R1_Only_Thermocouples)
Thermocouples_15min_B1R1 <- bind_cols(Thermocouples_All_15_min[[1]][,c(1:2)],Thermocouples_15min_B1R1)

Thermocouples_15min_B1R1 <- Thermocouples_15min_B1R1 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B1R1 <- Thermocouples_15min_B1R1 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)

#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13
Thermocouples_15min_B1R1[Thermocouples_15min_B1R1$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_14','Therm_Couple_17','Therm_Couple_18')] <- NA


#Berry Thermocouple 7 was found out of the berry and changed the 31/07 to another one,
#according to visual observation of the graph it might have come out the 27/07
Thermocouples_15min_B1R1[Thermocouples_15min_B1R1$Date_serial >= decimal_date(as.Date("2021-07-27")) & Thermocouples_15min_B1R1$Date_serial <= decimal_date(as.Date("2021-07-31")),'Therm_Couple_7'] <- NA

#Berry Thermocouple 1 was found wilted and changed the 31/07 to another one,
#according to visual observation of the graph it might have come out the 27/07
Thermocouples_15min_B1R1[Thermocouples_15min_B1R1$Date_serial >= decimal_date(as.Date("2021-07-27")) & Thermocouples_15min_B1R1$Date_serial <= decimal_date(as.Date("2021-07-31")),'Therm_Couple_1'] <- NA


#Berry Thermocouple 1 was found wilted and changed the 20/08 to another one,
#according to visual observation of the graph it might have come out the 20/08
Thermocouples_15min_B1R1[Thermocouples_15min_B1R1$Date_serial >= decimal_date(as.Date("2021-08-17")) & Thermocouples_15min_B1R1$Date_serial <= decimal_date(as.Date("2021-08-20")),'Therm_Couple_7'] <- NA


Thermocouples_15min_B1R1 <- Thermocouples_15min_B1R1 %>% select(-DATE,-Date_serial)

Thermocouples_All_15_min_Clean <- NULL
Thermocouples_All_15_min_Clean[[1]] <- NULL
Thermocouples_All_15_min_Clean[[1]] <- Thermocouples_15min_B1R1



Thermocouples_sel <- Thermocouples_All_15_min[[1]] %>% select(TIMESTAMP,Therm_Couple_19,Therm_Couple_20) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)





Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 8 | Month == 9)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



Thermocouples_15min_B1R1_Graph <- Thermocouples_All_15_min_Clean[[1]] %>% select(-Station)
Thermocouples_15min_B1R1_Graph <- Thermocouples_15min_B1R1_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B1R1_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


write.csv(Thermocouples_All_15_min_Clean[[1]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B1R1'))


###########################################
###########################################
#B1R4
###########################################
###########################################
Thermocouples_15min_B1R4 <- Thermocouples_All_15_min[[2]]
Thermocouples_15min_B1R4_Only_Thermocouples <- Thermocouples_15min_B1R4 %>% select_if(is.numeric)


Mean_rows <- rowMeans(Thermocouples_15min_B1R4_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B1R4_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B1R4_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B1R4_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B1R4_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B1R4_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B1R4 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B1R4) <- names(Thermocouples_15min_B1R4_Only_Thermocouples)
Thermocouples_15min_B1R4 <- bind_cols(Thermocouples_All_15_min[[2]][,c(1:2)],Thermocouples_15min_B1R4)


Thermocouples_15min_B1R4 <- Thermocouples_15min_B1R4 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B1R4 <- Thermocouples_15min_B1R4 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)

#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; then TC 17 was rescued on 07/31
Thermocouples_15min_B1R4[Thermocouples_15min_B1R4$Date_serial >= decimal_date(as.Date("2021-06-13")) & Thermocouples_15min_B1R4$Date_serial <= decimal_date(as.Date("2021-08-01")),'Therm_Couple_17'] <- NA
Thermocouples_15min_B1R4[Thermocouples_15min_B1R4$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_14','Therm_Couple_18')] <- NA


Thermocouples_15min_B1R4 <- Thermocouples_15min_B1R4 %>% select(-DATE,-Date_serial)


Thermocouples_All_15_min_Clean[[2]] <- NULL
Thermocouples_All_15_min_Clean[[2]] <- Thermocouples_15min_B1R4




Thermocouples_sel <- Thermocouples_15min_B1R4 %>% select(TIMESTAMP,Therm_Couple_11,Therm_Couple_9) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 9 |Month == 8)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



Thermocouples_15min_B1R4_Graph <- Thermocouples_All_15_min_Clean[[2]] %>% select(-Station)
Thermocouples_15min_B1R4_Graph <- Thermocouples_15min_B1R4_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B1R4_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()

write.csv(Thermocouples_All_15_min_Clean[[2]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B1R4'))


###########################################
###########################################
#B1R3
###########################################
###########################################
Thermocouples_15min_B1R3 <- Thermocouples_All_15_min[[3]]
Thermocouples_15min_B1R3 <- Thermocouples_15min_B1R3 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B1R3_Only_Thermocouples <- Thermocouples_15min_B1R3 %>% select_if(is.numeric)




Mean_rows <- rowMeans(Thermocouples_15min_B1R3_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B1R3_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B1R3_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B1R3_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B1R3_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B1R3_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B1R3 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B1R3) <- names(Thermocouples_15min_B1R3_Only_Thermocouples)
Thermocouples_15min_B1R3 <- bind_cols(Thermocouples_All_15_min[[3]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B1R3)


Thermocouples_15min_B1R3 <- Thermocouples_15min_B1R3 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B1R3 <- Thermocouples_15min_B1R3 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B1R3[Thermocouples_15min_B1R3$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_14','Therm_Couple_17','Therm_Couple_18')] <- NA

Thermocouples_15min_B1R3 <- Thermocouples_15min_B1R3 %>% select(-DATE,-Date_serial)


Thermocouples_All_15_min_Clean[[3]] <- NULL
Thermocouples_All_15_min_Clean[[3]] <- Thermocouples_15min_B1R3


Thermocouples_15min_B1R3_Graph <- Thermocouples_All_15_min_Clean[[3]] %>% select(-Station)
Thermocouples_15min_B1R3_Graph <- Thermocouples_15min_B1R3_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B1R3_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


write.csv(Thermocouples_All_15_min_Clean[[3]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B1R3'))


###########################################
###########################################
#B2R1
###########################################
###########################################
Thermocouples_15min_B2R1 <- Thermocouples_All_15_min[[4]]
Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B2R1_Only_Thermocouples <- Thermocouples_15min_B2R1 %>% select_if(is.numeric)




Mean_rows <- rowMeans(Thermocouples_15min_B2R1_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B2R1_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B2R1_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B2R1_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B2R1_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B2R1_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B2R1 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B2R1) <- names(Thermocouples_15min_B2R1_Only_Thermocouples)
Thermocouples_15min_B2R1 <- bind_cols(Thermocouples_All_15_min[[4]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B2R1)


Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_17','Therm_Couple_18')] <- NA

#Leaf from TC 19 was found completely burnt the 08/20, 
#according to visual examination of the graph it burnt the 9th of August
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-08-09")) & Thermocouples_15min_B2R1$Date_serial <= decimal_date(as.Date("2021-08-28")),'Therm_Couple_19'] <- NA

#Berries from TC 4 and 12 were found shrivelled the 08/09, 
#according to visual examination of the graph the shivelled the 3rd Spetember
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-09-03")) & Thermocouples_15min_B2R1$Date_serial <= decimal_date(as.Date("2021-09-8")),c('Therm_Couple_4','Therm_Couple_12')] <- NA



Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B2R1 %>% select(TIMESTAMP,Therm_Couple_12,Therm_Couple_4,Therm_Couple_11,Therm_Couple_3) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 9)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


Thermocouples_All_15_min_Clean[[4]] <- NULL
Thermocouples_All_15_min_Clean[[4]] <- Thermocouples_15min_B2R1


Thermocouples_15min_B2R1_Graph <- Thermocouples_All_15_min_Clean[[4]] %>% select(-Station)
Thermocouples_15min_B2R1_Graph <- Thermocouples_15min_B2R1_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B2R1_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


write.csv(Thermocouples_All_15_min_Clean[[4]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B2R1'))


###########################################
###########################################
#B2R2
###########################################
###########################################
Thermocouples_15min_B2R2 <- Thermocouples_All_15_min[[5]]
Thermocouples_15min_B2R2 <- Thermocouples_15min_B2R2 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B2R2_Only_Thermocouples <- Thermocouples_15min_B2R2 %>% select_if(is.numeric)



Mean_rows <- rowMeans(Thermocouples_15min_B2R2_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B2R2_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B2R2_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B2R2_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B2R2_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B2R2_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B2R1 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B2R1) <- names(Thermocouples_15min_B2R1_Only_Thermocouples)
Thermocouples_15min_B2R1 <- bind_cols(Thermocouples_All_15_min[[4]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B2R1)


Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 13;17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_17','Therm_Couple_18')] <- NA

#Leaf from TC 19 was found completely burnt the 08/20, 
#according to visual examination of the graph it burnt the 9th of August
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-08-09")) & Thermocouples_15min_B2R1$Date_serial <= decimal_date(as.Date("2021-08-28")),'Therm_Couple_19'] <- NA

#Berries from TC 4 and 12 were found shrivelled the 08/09, 
#according to visual examination of the graph the shivelled the 3rd Spetember
Thermocouples_15min_B2R1[Thermocouples_15min_B2R1$Date_serial >= decimal_date(as.Date("2021-09-03")) & Thermocouples_15min_B2R1$Date_serial <= decimal_date(as.Date("2021-09-8")),c('Therm_Couple_4','Therm_Couple_12')] <- NA



Thermocouples_15min_B2R1 <- Thermocouples_15min_B2R1 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B2R1 %>% select(TIMESTAMP,Therm_Couple_12,Therm_Couple_4,Therm_Couple_11,Therm_Couple_3) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 9)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



Thermocouples_All_15_min_Clean[[4]] <- NULL
Thermocouples_All_15_min_Clean[[4]] <- Thermocouples_15min_B2R1


Thermocouples_15min_B2R1_Graph <- Thermocouples_All_15_min_Clean[[4]] %>% select(-Station)
Thermocouples_15min_B2R1_Graph <- Thermocouples_15min_B2R1_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B2R1_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


###########################################
###########################################
#B2R3
###########################################
###########################################
Thermocouples_15min_B2R3 <- Thermocouples_All_15_min[[6]]
Thermocouples_15min_B2R3 <- Thermocouples_15min_B2R3 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B2R3_Only_Thermocouples <- Thermocouples_15min_B2R3 %>% select_if(is.numeric)



Mean_rows <- rowMeans(Thermocouples_15min_B2R3_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B2R3_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B2R3_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B2R3_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B2R3_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B2R3_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B2R3 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B2R3) <- names(Thermocouples_15min_B2R3_Only_Thermocouples)
Thermocouples_15min_B2R3 <- bind_cols(Thermocouples_All_15_min[[6]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B2R3)


Thermocouples_15min_B2R3 <- Thermocouples_15min_B2R3 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B2R3 <- Thermocouples_15min_B2R3 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 13; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B2R3[Thermocouples_15min_B2R3$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_17','Therm_Couple_18')] <- NA

#TC 4 was found outside its berry the 08/06 and put again in a berry
#Remove all measurements prior that date
Thermocouples_15min_B2R3[Thermocouples_15min_B2R3$Date_serial <= decimal_date(as.Date("2021-08-06")),'Therm_Couple_4'] <- NA



Thermocouples_15min_B2R3 <- Thermocouples_15min_B2R3 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B2R3 %>% select(TIMESTAMP,Therm_Couple_12,Therm_Couple_4,Therm_Couple_11,Therm_Couple_3) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 9)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


Thermocouples_All_15_min_Clean[[6]] <- NULL
Thermocouples_All_15_min_Clean[[6]] <- Thermocouples_15min_B2R3


Thermocouples_15min_B2R3_Graph <- Thermocouples_All_15_min_Clean[[6]] %>% select(-Station)
Thermocouples_15min_B2R3_Graph <- Thermocouples_15min_B2R3_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B2R3_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



write.csv(Thermocouples_All_15_min_Clean[[6]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B2R3'))


###########################################
###########################################
#B3R1
###########################################
###########################################
Thermocouples_15min_B3R1 <- Thermocouples_All_15_min[[7]]
Thermocouples_15min_B3R1 <- Thermocouples_15min_B3R1 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B3R1_Only_Thermocouples <- Thermocouples_15min_B3R1 %>% select_if(is.numeric)


Mean_rows <- rowMeans(Thermocouples_15min_B3R1_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B3R1_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B3R1_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B3R1_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B3R1_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B3R1_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B3R1 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B3R1) <- names(Thermocouples_15min_B3R1_Only_Thermocouples)
Thermocouples_15min_B3R1 <- bind_cols(Thermocouples_All_15_min[[7]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B3R1)


Thermocouples_15min_B3R1 <- Thermocouples_15min_B3R1 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B3R1 <- Thermocouples_15min_B3R1 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B3R1[Thermocouples_15min_B3R1$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_17','Therm_Couple_18')] <- NA

#TC 11 (berry) showed erratic behaviour, was deleted from the dataset
Thermocouples_15min_B3R1[,'Therm_Couple_11'] <- NA



Thermocouples_15min_B3R1 <- Thermocouples_15min_B3R1 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B3R1 %>% select(TIMESTAMP,Therm_Couple_12,Therm_Couple_11) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 8)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



Thermocouples_All_15_min_Clean[[7]] <- NULL
Thermocouples_All_15_min_Clean[[7]] <- Thermocouples_15min_B3R1


Thermocouples_15min_B3R1_Graph <- Thermocouples_All_15_min_Clean[[7]] %>% select(-Station)
Thermocouples_15min_B3R1_Graph <- Thermocouples_15min_B3R1_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B3R1_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


write.csv(Thermocouples_All_15_min_Clean[[7]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B3R1'))


###########################################
###########################################
#B3R2
###########################################
###########################################
Thermocouples_15min_B3R2 <- Thermocouples_All_15_min[[8]]
Thermocouples_15min_B3R2 <- Thermocouples_15min_B3R2 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B3R2_Only_Thermocouples <- Thermocouples_15min_B3R2 %>% select_if(is.numeric)


Mean_rows <- rowMeans(Thermocouples_15min_B3R2_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B3R2_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B3R2_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B3R2_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B3R2_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B3R2_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B3R2 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B3R2) <- names(Thermocouples_15min_B3R2_Only_Thermocouples)
Thermocouples_15min_B3R2 <- bind_cols(Thermocouples_All_15_min[[8]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B3R2)


Thermocouples_15min_B3R2 <- Thermocouples_15min_B3R2 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B3R2 <- Thermocouples_15min_B3R2 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 14, 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B3R2[Thermocouples_15min_B3R2$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_14','Therm_Couple_17','Therm_Couple_18')] <- NA


Thermocouples_15min_B3R2 <- Thermocouples_15min_B3R2 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B3R1 %>% select(TIMESTAMP,Therm_Couple_12,Therm_Couple_11) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 8)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()



Thermocouples_All_15_min_Clean[[8]] <- NULL
Thermocouples_All_15_min_Clean[[8]] <- Thermocouples_15min_B3R2


Thermocouples_15min_B3R2_Graph <- Thermocouples_All_15_min_Clean[[8]] %>% select(-Station)
Thermocouples_15min_B3R2_Graph <- Thermocouples_15min_B3R2_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B3R2_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


write.csv(Thermocouples_All_15_min_Clean[[8]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B3R2'))


###########################################
###########################################
#B3R3
###########################################
###########################################
Thermocouples_15min_B3R3 <- Thermocouples_All_15_min[[9]]
Thermocouples_15min_B3R3 <- Thermocouples_15min_B3R3 %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6) #Selecting from June onwards cause we don't have measurements before that

Thermocouples_15min_B3R3_Only_Thermocouples <- Thermocouples_15min_B3R3 %>% select_if(is.numeric)


Mean_rows <- rowMeans(Thermocouples_15min_B3R3_Only_Thermocouples, na.rm = TRUE)
SD_rows <- apply(Thermocouples_15min_B3R3_Only_Thermocouples,1,sd, na.rm = TRUE)


Matrix_Clean <-matrix(ncol = ncol(Thermocouples_15min_B3R3_Only_Thermocouples), nrow = nrow(Thermocouples_15min_B3R3_Only_Thermocouples))

cut <- 2
for(i in 1:nrow(Thermocouples_15min_B3R3_Only_Thermocouples)){
  
  Matrix_Clean[i,] <- apply(Thermocouples_15min_B3R3_Only_Thermocouples[i,],2,function (x) if(is.na(x)) {x=NA} 
                            else if (x > Mean_rows[i]+cut*SD_rows[i]) {x = NA} 
                            else if (x < Mean_rows[i]-cut*SD_rows[i]) {x = NA}
                            else{x = x})
  
}

Thermocouples_15min_B3R3 <- as.data.frame(Matrix_Clean)
names(Thermocouples_15min_B3R3) <- names(Thermocouples_15min_B3R3_Only_Thermocouples)
Thermocouples_15min_B3R3 <- bind_cols(Thermocouples_All_15_min[[9]][,c(1:2)] %>% filter(TIMESTAMP %>% as.Date() %>% lubridate::month() >= 6),Thermocouples_15min_B3R3)


Thermocouples_15min_B3R3 <- Thermocouples_15min_B3R3 %>% mutate(DATE = as.Date(TIMESTAMP, tz = "America/Los_Angeles"), .after = TIMESTAMP)
Thermocouples_15min_B3R3 <- Thermocouples_15min_B3R3 %>% mutate(Date_serial = decimal_date(DATE), .after = DATE)


#Leaves where Thermocouples were placed in the North (channels 13, 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 
Thermocouples_15min_B3R3[Thermocouples_15min_B3R3$Date_serial >= decimal_date(as.Date("2021-06-13")),c('Therm_Couple_13','Therm_Couple_17','Therm_Couple_18')] <- NA

#TC 12 (Berry) broke 08/10; no need to delete data, when broken output <- NA


Thermocouples_15min_B3R3 <- Thermocouples_15min_B3R3 %>% select(-DATE,-Date_serial)



Thermocouples_sel <- Thermocouples_15min_B3R3 %>% select(TIMESTAMP,Therm_Couple_5,Therm_Couple_9,Therm_Couple_6,Therm_Couple_10, Therm_Couple_12) %>% melt(id.vars = "TIMESTAMP")
Thermocouples_sel$TIMESTAMP <- ymd_hms(Thermocouples_sel$TIMESTAMP)
Thermocouples_sel$TIMESTAMP <- as.POSIXct(Thermocouples_sel$TIMESTAMP)


Thermocouples_sel <- Thermocouples_sel %>% mutate(Month = month(TIMESTAMP), .after = "TIMESTAMP")

Thermocouples_Month <- Thermocouples_sel %>% filter(Month == 9)

ggplot(Thermocouples_Month, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()


Thermocouples_All_15_min_Clean <- NULL
Thermocouples_All_15_min_Clean[[9]] <- NULL
Thermocouples_All_15_min_Clean[[9]] <- Thermocouples_15min_B3R3


Thermocouples_15min_B3R3_Graph <- Thermocouples_All_15_min_Clean[[9]] %>% select(-Station)
Thermocouples_15min_B3R3_Graph <- Thermocouples_15min_B3R3_Graph %>% melt(id.var = 'TIMESTAMP')

ggplot(Thermocouples_15min_B3R3_Graph, aes(x = TIMESTAMP, y = value, colour = variable)) + geom_line()

write.csv(Thermocouples_All_15_min_Clean[[9]],paste0(Dir_Output,'Thermocouples_15_min_Clean_B3R3'))


###########################################
###########################################









