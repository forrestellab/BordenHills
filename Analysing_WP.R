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
#library(googlesheets4)



Dir <- c("/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Physiology/WaterPotential/CSV/")
setwd(Dir)
Dir_List <- list.dirs(path = Dir, full.names = TRUE, recursive = TRUE)

#Listing files
WP_files <- list.files(Dir_List,pattern='[.]csv') #all csv files in the folder

Predawn_WP <- WP_files[grep("Pre",WP_files)] #Predawn csv files


Predawn_List <- NULL
for(i in 1:length(Predawn_WP)){
  
  #reading and cleaning dataframes
  File_in_use <- read.csv(Predawn_WP[i])
  Predawn_List[[i]] <- File_in_use %>% select(c(date,block_id,vine,leaf_wp_bar))
  Predawn_List[[i]] <- Predawn_List[[i]][!(is.na(Predawn_List[[i]]$date) | Predawn_List[[i]]$date==""), ]
  
  #configurating columns
  Predawn_List[[i]]$date <- as.Date(Predawn_List[[i]]$date, format = "%m-%d-%y")
  Predawn_List[[i]]$block_id <- as.character(Predawn_List[[i]]$block_id)
  Predawn_List[[i]]$vine <- as.character(Predawn_List[[i]]$vine)
  Predawn_List[[i]]$leaf_wp_bar <- as.numeric(Predawn_List[[i]]$leaf_wp_bar)
  
}

#converting list into a single df
Predawn_df <- do.call(bind_rows,Predawn_List)
#calculating -MPa from +bar
Predawn_df <- Predawn_df %>% mutate(Predawn_WP = -leaf_wp_bar/10, .after = leaf_wp_bar) %>% 
  select(-leaf_wp_bar) %>% 
  mutate(Key = paste(date,block_id,vine, sep = "_"), .before = date)



##############################
###Same for Midday WP########
Midday_WP <- WP_files[grep("Mid",WP_files)] #MIdday csv files


Midday_List <- NULL
for(i in 1:length(Midday_WP)){
  
  #reading and cleaning dataframes
  File_in_use <- read.csv(Midday_WP[i])
  Midday_List[[i]] <- File_in_use %>% select(c(date,block_id,vine,stem_wp_bar,leaf_wp_bar,leaf_temp))
  Midday_List[[i]] <- Midday_List[[i]][!(is.na(Midday_List[[i]]$date) | Midday_List[[i]]$date==""), ]
  
  #configurating columns
  Midday_List[[i]]$date <- as.Date(Midday_List[[i]]$date, format = "%m-%d-%y")
  Midday_List[[i]]$block_id <- as.character(Midday_List[[i]]$block_id)
  Midday_List[[i]]$vine <- as.character(Midday_List[[i]]$vine)
  Midday_List[[i]]$stem_wp_bar <- as.numeric(Midday_List[[i]]$stem_wp_bar)
  Midday_List[[i]]$leaf_wp_bar <- as.numeric(Midday_List[[i]]$leaf_wp_bar)
  Midday_List[[i]]$leaf_temp <- as.numeric(Midday_List[[i]]$leaf_temp)

}

#converting list into a single df
Midday_df <- do.call(bind_rows,Midday_List)

#calculating -MPa from +bar
Midday_df <- Midday_df %>% mutate(Midday_Stem_WP = -stem_wp_bar/10, .after = stem_wp_bar) %>% 
  mutate(Midday_Leaf_WP = -leaf_wp_bar/10, .after = leaf_wp_bar) %>% 
  mutate(Midday_Leaf_T = leaf_temp, .after = leaf_temp) %>% 
  select(-c(stem_wp_bar,leaf_wp_bar,leaf_temp)) %>% 
  mutate(Key = paste(date,block_id,vine, sep = "_"), .before = date)


#Merging predawn and midday
BH_WP_All <- merge(Predawn_df, Midday_df, by.x = "Key", by.y = "Key", all.x = TRUE, all.y = TRUE)

#Arranging the BH_WP_All df
BH_WP_All <- BH_WP_All %>% select(-c(date.x,block_id.x,vine.x,date.y,block_id.y,vine.y))

#regaining date, block and vine from the Key
BH_WP_All <- BH_WP_All %>% mutate(Date = substr(Key,1,10), .before = Key) %>% 
  mutate(Block = substr(Key,12,18), .after = Date) %>% 
  mutate(Vine = substr(Key,20,nchar(Key)), .after = Block) %>% 
  mutate(Treatment = substr(Block,1,2), .after = Block) %>% 
  select(-Key)

BH_WP_All$Date <- as.Date(BH_WP_All$Date)


write.csv(BH_WP_All, file = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/BordenHills2021_Complete/Water_Potential_2021.csv",
          row.names = FALSE, col.names = TRUE)


  ggplot(data = BH_WP_All, aes(x = Date, y = Midday_Leaf_WP)) +
  geom_boxplot(aes(fill = Treatment,
                   group = interaction(factor(Date), Treatment)),  width =2.5, position=position_dodge(3), outlier.size=0.75) + 
    ylab(bquote(""*Psi[Midday]~ "(MPa)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2021-06-01","2021-09-15")),date_breaks = "2 week", date_labels = "%d-%b") +
  
  annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') +
  
  annotate("rect", xmin =  as.Date("2021-09-07"), xmax = as.Date("2021-09-09") , ymin = -Inf, ymax = Inf,
           alpha = .2, fill = "red") +
  annotate("text", x = as.Date("2021-09-07"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW3", size = 3, colour = 'red') +
  theme_bw() 

  ggsave('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Physiology/MiddayPsi.pdf')
  
  
  
  ggplot(data = BH_WP_All, aes(x = Date, y = Predawn_WP)) +
    geom_boxplot(aes(fill = Treatment,
                     group = interaction(factor(Date), Treatment)),  width =2.5, position=position_dodge(3), outlier.size=0.75) + 
    ylab(bquote(""*Psi[Predawn]~ "(MPa)")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(limits = as.Date(c("2021-06-01","2021-09-15")),date_breaks = "2 week", date_labels = "%d-%b") +
    
    annotate("rect", xmin =  as.Date("2021-06-16"), xmax = as.Date("2021-06-18") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-06-16"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW1", size = 3, colour = 'red') +
    
    annotate("rect", xmin =  as.Date("2021-07-09"), xmax = as.Date("2021-07-11") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-07-09"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW2", size = 3, colour = 'red') +
    
    annotate("rect", xmin =  as.Date("2021-09-07"), xmax = as.Date("2021-09-09") , ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "red") +
    annotate("text", x = as.Date("2021-09-07"), hjust = 1.16, y = Inf, vjust = 1.16, label = "HW3", size = 3, colour = 'red') +
    theme_bw() 
  
  ggsave('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Physiology/PredawnPsi.pdf')
  
  
