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
setwd('/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/') #Folder where you find the outputs from the compiling script "Compiling_Met_Thermo_Files.R" 
Dir <- getwd()

# Setting Output directory
Dir_Output <- c("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Thermocouples_15min/")

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
  
  
  #adding a floor to have time records every 15 minutes
  Thermocouples_Selection <- Thermocouples_Selection %>% add_column(Floor_date = floor_date(Thermocouples_Selection$TIMESTAMP, "15 mins"),.after = "TIMESTAMP")
  
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
  
  write.csv(Thermocouples_15min_Ave, file = paste0(Dir_Output,paste(Stations_List[i],"Thermocouples.csv",sep = "_")),row.names = FALSE)
  
  #Saving All Stations in one list
  Thermocouples_All_15_min[[i]] <- Thermocouples_15min_Ave
  
}

#Consolidating the list
Thermocouples_All_15_min <- do.call(bind_rows,Thermocouples_All_15_min)

#save Rdat
setwd(Dir_Output)
save(Thermocouples_All_15_min,file = "All_Thermocouples_15_min.Rdata")




###########################################
###########################################
load("All_Thermocouples_15_min.Rdata")




Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

for(z in 1:length(Stations_List)){
  
  Station_x = Stations_List[z]
  
  TC_B1R1 <- Thermocouples_All_15_min %>% filter(Station == Station_x)
  class(TC_B1R1$TIMESTAMP)
  
  TC_B1R1 <- add_column(TC_B1R1, Hour = lubridate::hour(TC_B1R1$TIMESTAMP), .after = "TIMESTAMP")
  
  TC_B1R1_3PM <- TC_B1R1 %>% filter(Hour == 15)
  TC_B1R1_3PM <- add_column(TC_B1R1_3PM, Day = lubridate::date(TC_B1R1_3PM$TIMESTAMP), .after = "TIMESTAMP")
  
  
  Date_minutes_DAY <- paste(lubridate::month(TC_B1R1_3PM$TIMESTAMP),lubridate::day(TC_B1R1_3PM$TIMESTAMP),
                            lubridate::minute(TC_B1R1_3PM$TIMESTAMP),sep = "-")
  
  
  
  Unique_Days <- lubridate::date(TC_B1R1_3PM$TIMESTAMP) %>% unique()
  
  
  var <- names(TC_B1R1_3PM)[grep("Therm_Couple",names(TC_B1R1_3PM))]
  hola <- melt(TC_B1R1_3PM,id.vars = "Day", measure.vars = var)
  hola$Day <- as.factor(hola$Day)
  
  ggplot(hola, aes(x=Day, y=value)) + geom_boxplot() + labs(title = 'B1R1 3 PM')
  
  
  Only_Thermocouples <- TC_B1R1_3PM[,c(6:ncol(TC_B1R1_3PM))] %>% as.data.frame()
  #Only_Thermocouples <- Only_Thermocouples %>% select(-Therm_Couple_6)
  
  Mean_rows <- rowMeans(Only_Thermocouples, na.rm = TRUE)
  SD_rows <- apply(Only_Thermocouples,1,sd, na.rm = TRUE)
  
  
  
  
  
  
  # Warning_TC <- function(TC_temp,mean,SD,cut) {
  #   
  #   #if(is.na(TC_temp) | is.na())
  #   if(TC_temp < mean-(cut*SD) | TC_temp > mean+(cut*SD)){
  #     Warning <- 1
  #     }else{Warning <- 0}
  #   
  #   return(Warning)
  # }
  
  
  Warning_TC <- function(TC_temp,mean,SD,cut) {
    
    #if(is.na(TC_temp) | is.na())
    if(TC_temp < mean-(cut*SD)){
      Warning <- c("Low")
    }else if(TC_temp > mean+(cut*SD)){
      Warning <- c("High")
    }else{Warning <- c("Good")}
    return(Warning)
  }
  
  
  cut = 3
  
  matrix_Day <- matrix(ncol = ncol(Only_Thermocouples), nrow = nrow(Only_Thermocouples), data = NA)
  
  for(i in 1:ncol(Only_Thermocouples)){
    matrix_Day[,i] <- mapply(Warning_TC,Only_Thermocouples[i],Mean_rows,SD_rows, cut)
  }
  #day_score <- colSums(matrix_Day)
  
  for(i in 1:nrow(matrix_Day)){
    for(j in 1:ncol(matrix_Day)){
      
      matrix_Day[i,j] <- paste("Day",matrix_Day[i,j],sep = "_") 
    }
  }
  
  
  
  ### Night Score
  
  
  TC_B1R1_11PM <- TC_B1R1 %>% filter(Hour == 23)
  TC_B1R1_11PM <- add_column(TC_B1R1_11PM, Day = lubridate::date(TC_B1R1_11PM$TIMESTAMP), .after = "TIMESTAMP")
  
  chao <- melt(TC_B1R1_11PM,id.vars = "Day", measure.vars = var)
  chao$Day <- as.factor(chao$Day)
  
  Date_minutes_NIGHT <- paste(lubridate::month(TC_B1R1_11PM$TIMESTAMP),lubridate::day(TC_B1R1_11PM$TIMESTAMP),
                              lubridate::minute(TC_B1R1_11PM$TIMESTAMP),sep = "-")
  
  
  
  
  Only_Thermocouples_night <- TC_B1R1_11PM[,c(6:ncol(TC_B1R1_11PM))] %>% as.data.frame()
  #Only_Thermocouples_night <- Only_Thermocouples_night %>% select(-Therm_Couple_6)
  
  Mean_rows <- rowMeans(Only_Thermocouples_night, na.rm = TRUE)
  SD_rows <- apply(Only_Thermocouples_night,1,sd, na.rm = TRUE)
  
  matrix_Night <- matrix(ncol = ncol(Only_Thermocouples_night), nrow = nrow(Only_Thermocouples_night), data = NA)
  
  cut = 3
  
  for(i in 1:ncol(Only_Thermocouples_night)){
    matrix_Night[,i] <- mapply(Warning_TC,Only_Thermocouples_night[i],Mean_rows,SD_rows, cut)
  }
  
  
  
  
  #night_score <- colSums(matrix_Night)
  for(i in 1:nrow(matrix_Night)){
    for(j in 1:ncol(matrix_Night)){
      
      matrix_Night[i,j] <- paste("Night",matrix_Night[i,j],sep = "_") 
    }
  }
  
  
  #Checking what days and minutes correspond to each subset of day and night
  match_delete_Day <- which(is.na(match(Date_minutes_DAY,Date_minutes_NIGHT)))
  match_delete_Night <- which(is.na(match(Date_minutes_NIGHT,Date_minutes_DAY)))
  
  
  ##We need to clean matrix_Day and matrix_Night so they match each other in length and TimeStamp
  
  if(length(match_delete_Day)!=0){
    matrix_Day <- matrix_Day[-c(match_delete_Day),]}
  
  if(length(match_delete_Night)!=0){
    matrix_Night <- matrix_Night[-c(match_delete_Night),]}
  
  
  Comparison <- matrix(ncol = ncol(matrix_Day), nrow = nrow(matrix_Day), data = NA)
  
  # for(i in 1:nrow(matrix_Day)){
  #   
  #   for(j in 1:ncol(matrix_Night)){
  #     
  #     if(matrix_Day[i,j] == 1 & matrix_Night[i,j] == 1){
  #     
  #     Comparison[i,j] <- 2
  #     }else if(matrix_Day[i,j] == 1 & matrix_Night[i,j] == 0){
  #     Comparison[i,j] <- 1
  #     }else if (matrix_Day[i,j] == 0 & matrix_Night[i,j] == 0){
  #     Comparison[i,j] <- 0
  #   }
  #     
  #   }
  # 
  # }
  
  for(i in 1:nrow(matrix_Day)){
    
    for(j in 1:ncol(matrix_Night)){
      
      Comparison[i,j] <- paste(matrix_Day[i,j],matrix_Night[i,j],sep="/")
      
      
    }
  }
  
  
  
  Comparison <- Comparison %>% as.data.frame()
  #unique(Comparison$V12)
  
  
  Comparison <- Comparison %>% as.data.frame()
  #Comparison <- add_column(Comparison, Date = TC_B1R1_3PM$Day, .before = "V1")
  
  
  
  
  Unique_options <- c("Day_Good/Night_Good", "Day_Good/Night_High", "Day_Good/Night_Low",  "Day_Low/Night_Low", 
                      "Day_Low/Night_Good",  "Day_High/Night_Low",  "Day_Low/Night_High",  "Day_High/Night_Good",
                      "Day_High/Night_High")
  
  matrix_score <- matrix(nrow = length(Unique_options), ncol = 20, data = NA)
  
  for(j in 1:20){
    for(i in 1:length(Unique_options)){
      matrix_score[i,j] <- sum(str_count(Comparison[,j], pattern = Unique_options[i]))
    }
    matrix_score[,j] <- as.numeric(matrix_score[,j])
  }
  
  
  n = 20
  ThermoCouples_names <- matrix(nrow = 1, ncol = n, data = NA)
  for(j in 1:n){
    ThermoCouples_names[1,j] <- paste("Therm_Couple",as.character(j),sep = "_")  
  }
  
  matrix_score <- matrix_score %>% as.data.frame()
  colnames(matrix_score) <- ThermoCouples_names
  
  #matrix_score <- lapply(matrix_score, as.numeric) %>% unlist()
  
  matrix_score <- add_column(matrix_score, Options = Unique_options, .before = "Therm_Couple_1")
  
  
  
  nico <- melt(matrix_score, id.vars = "Options")
  nico$Options <- as.factor(nico$Options) 
  
  
  ggplot(data = nico, aes(x=value,group=Options,fill=Options))+geom_density(adjust=1,alpha=.4) + 
    labs(title = paste(Station_x,"(± 3 SD)",sep = " ")) + xlab(paste0("Score (n = ",nrow(matrix_Day),")")) + xlim(0, nrow(matrix_Day))
  
  ggsave(paste0("./Figures/Density_TC_",Station_x,".pdf"))
  
  worst_score <- nico %>% filter(Options == "Day_High/Night_Low")
  
  worst_score <- worst_score %>% mutate(value = (value/nrow(matrix_Day))*100)
  
  
  
  #ggplot(data = worst_score, aes(x=value,group=Options,fill=Options))+geom_density(adjust=.25,alpha=.4) + 
  #labs(title = "B1R1 - Day High/Night Low (± 3SD)") + xlab("Score (n = 136)") + xlim(0, 140)
  
  
  #Figure
  #plot(x=c(1:20),worst_score$value, main  = "B1R1-Day High/Night Low", xlab = "Thermocouples Channels", ylab = "Score (n = 136)")
  
  ggplot(data = worst_score, aes(x=c(1:20), y = value)) + geom_line() + geom_point() + 
    labs(title = paste0(Station_x," - Day High/Night Low (± 3SD)")) + xlab("Thermocouples Channels") + ylab(paste0("Score % (n = ",nrow(matrix_Day),")"))
  
  ggsave(paste0("./Figures/Worst_Score_",Station_x,".pdf"))
  
  
}















chao <- add_column(chao, value2 = NA, .after = "value")
chao <- add_column(chao, value3 = NA, .after = "value2")
chao$value2[chao$variable == "Therm_Couple_13"]  <- chao$value[chao$variable == "Therm_Couple_13"]
#chao$value2[chao$variable == "Therm_Couple_7"]  <- chao$value[chao$variable == "Therm_Couple_7"]
#chao$value2[chao$variable == "Therm_Couple_14"]  <- chao$value[chao$variable == "Therm_Couple_14"]

chao$value3[chao$variable == "Therm_Couple_14"]  <- chao$value[chao$variable == "Therm_Couple_14"]
chao$value3[chao$variable == "Therm_Couple_15"]  <- chao$value[chao$variable == "Therm_Couple_15"]

ggplot(chao, aes(x=Day, y=value)) + geom_boxplot(outlier.size = 0.5) + geom_point(aes(x=Day, y=value2), color = "blue", size = 1.5) + 
  geom_point(aes(x=Day, y=value3), color = "red", size = 1.5) + labs(title = "B3R3 3 AM")

ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_TC/B3R3_3AM.pdf")

hola <- add_column(hola, value2 = NA, .after = "value")
hola <- add_column(hola, value3 = NA, .after = "value2")
hola$value2[hola$variable == "Therm_Couple_14"]  <- hola$value[hola$variable == "Therm_Couple_14"]
hola$value2[hola$variable == "Therm_Couple_15"]  <- hola$value[hola$variable == "Therm_Couple_15"]

#hola$value3[hola$variable == "Therm_Couple_5"]  <- hola$value[hola$variable == "Therm_Couple_5"]
hola$value3[hola$variable == "Therm_Couple_13"]  <- hola$value[hola$variable == "Therm_Couple_13"]
#hola$value3[hola$variable == "Therm_Couple_14"]  <- hola$value[hola$variable == "Therm_Couple_14"]

ggplot(hola, aes(x=Day, y=value)) + geom_boxplot(outlier.size = 0.5) + geom_point(aes(x=Day, y=value2), color = "red", size = 1.5) + 
  geom_point(aes(x=Day, y=value3), color = "blue", size = 1.5) + labs(title = "B3R2 3 PM")


ggsave("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Figures_TC/B3R3_3PM.pdf")

