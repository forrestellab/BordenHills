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



########################################
#### Baseline June Measurements ########
########################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/04 June 2021 Midday")

#Reading only file from Licor EJF1 midday
BL_June_PreDawn_EJF1 <- read.csv("CSV/2021-06-04-BH_Middays_EJF1.csv")
names(BL_June_PreDawn_EJF1) <- BL_June_PreDawn_EJF1[13,]
BL_June_PreDawn_EJF1 <- BL_June_PreDawn_EJF1[-seq(1,14,1),]

#Reading only file from EJF2 midday
BL_June_PreDawn_EJF2 <- read.csv("CSV/2021-06-04-BH_Middays_EJF2.csv")
names(BL_June_PreDawn_EJF2) <- BL_June_PreDawn_EJF2[13,]
BL_June_PreDawn_EJF2 <- BL_June_PreDawn_EJF2[-seq(1,14,1),]

# Merging all dataframes
BaseLineJune_Midday <- bind_rows(BL_June_PreDawn_EJF1,BL_June_PreDawn_EJF2)

DATE <- as.Date(substr(BaseLineJune_Midday$date,1,8), format = "%Y%m%d")
BaseLineJune_Midday <- add_column(BaseLineJune_Midday, DATE = DATE, .before = "date")


#adding constants
BaseLineJune_Midday <- add_column(BaseLineJune_Midday, HeatWave = 0, .before = "BLOCK")
BaseLineJune_Midday <- add_column(BaseLineJune_Midday, TimeOfDay = "Midday", .after = "HeatWave")
BaseLineJune_Midday <- add_column(BaseLineJune_Midday, DayOfHW = "BaseLine", .after = "TimeOfDay")


########################################
#### Reading Pre HeatWave data #########
########################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/15 June 2021 PreHeatWave")

#Reading second file from Licor EJF1 Predawn
PreHW1_PreDawn_EJF1_2 <- read.csv("CSV/2021-06-15-BH_PreDawn_EJF1_v2.csv")
names(PreHW1_PreDawn_EJF1_2) <- PreHW1_PreDawn_EJF1_2[13,]
PreHW1_PreDawn_EJF1_2 <- PreHW1_PreDawn_EJF1_2[-seq(1,14,1),]

#Reading only file from Licor McElrone Predawn
PreHW1_PreDawn_Mc_1 <- read.csv("CSV/2021-06-15-0529_BH_PreDawn_McElrone.csv")
names(PreHW1_PreDawn_Mc_1) <- PreHW1_PreDawn_Mc_1[13,]
PreHW1_PreDawn_Mc_1 <- PreHW1_PreDawn_Mc_1[-seq(1,14,1),]


# Merging all dataframes
Pre_HW1_PreDawn <- bind_rows(PreHW1_PreDawn_EJF1_2,PreHW1_PreDawn_Mc_1)

DATE <- as.Date(substr(Pre_HW1_PreDawn$date,1,8), format = "%Y%m%d")
Pre_HW1_PreDawn <- add_column(Pre_HW1_PreDawn, DATE = DATE, .before = "date")

#adding constants
Pre_HW1_PreDawn <- add_column(Pre_HW1_PreDawn, HeatWave = 1, .before = "BLOCK")
Pre_HW1_PreDawn <- add_column(Pre_HW1_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
Pre_HW1_PreDawn <- add_column(Pre_HW1_PreDawn, DayOfHW = "PreHW", .after = "TimeOfDay")


###Reading midday measurements

#Reading only file from Licor EJF1 Midday
PreHW1_Midday_EJF1 <- read.csv("CSV/2021-06-15-1334_BH_MidDay_EJF1.csv")
names(PreHW1_Midday_EJF1) <- PreHW1_Midday_EJF1[13,]
PreHW1_Midday_EJF1 <- PreHW1_Midday_EJF1[-seq(1,14,1),]

#Reading only file from Licor McElrond Midday
PreHW1_Midday_Mc  <- read.csv("CSV/2021-06-15-BH_MidDay_McElrone.csv")
names(PreHW1_Midday_Mc) <- PreHW1_Midday_Mc[13,]
PreHW1_Midday_Mc <- PreHW1_Midday_Mc[-seq(1,14,1),]


# Merging all dataframes
Pre_HW1_MidDay <- bind_rows(PreHW1_Midday_EJF1,PreHW1_Midday_Mc)

DATE <- as.Date(substr(Pre_HW1_MidDay$date,1,8), format = "%Y%m%d")
Pre_HW1_MidDay <- add_column(Pre_HW1_MidDay, DATE = DATE, .before = "date")

#adding constants
Pre_HW1_MidDay <- add_column(Pre_HW1_MidDay, HeatWave = 1, .before = "BLOCK")
Pre_HW1_MidDay <- add_column(Pre_HW1_MidDay, TimeOfDay = "Midday", .after = "HeatWave")
Pre_HW1_MidDay <- add_column(Pre_HW1_MidDay, DayOfHW = "PreHW", .after = "TimeOfDay")

## Merging predawn and midday for pre HW 1

Pre_HW1 <- bind_rows(Pre_HW1_PreDawn,Pre_HW1_MidDay)



######################################################
################# HW1 ###############################
######################################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/17 June 2021 HeatWave')


#Reading only file from Licor EJF1 Predawn
HW1_PreDawn_EJF1 <- read.csv("CSV/2021-06-17-PreDawn_EJF1.csv")
names(HW1_PreDawn_EJF1) <- HW1_PreDawn_EJF1[13,]
HW1_PreDawn_EJF1 <- HW1_PreDawn_EJF1[-seq(1,14,1),]


#Reading only file from Licor McElrone Predawn
HW1_PreDawn_Mc <- read.csv("CSV/2021-06-17-PreDawn_McElrone.csv")
names(HW1_PreDawn_Mc) <- HW1_PreDawn_Mc[13,]
HW1_PreDawn_Mc <- HW1_PreDawn_Mc[-seq(1,14,1),]
#deleting rows with empty cells
HW1_PreDawn_Mc <- HW1_PreDawn_Mc[!apply(HW1_PreDawn_Mc == "", 1, all), ]  

# Merging all dataframes
HW1_PreDawn <- bind_rows(HW1_PreDawn_EJF1,HW1_PreDawn_Mc)

DATE <- as.Date(substr(HW1_PreDawn$date,1,8), format = "%Y%m%d")
HW1_PreDawn <- add_column(HW1_PreDawn, DATE = DATE, .before = "date")

#adding constants
HW1_PreDawn <- add_column(HW1_PreDawn, HeatWave = 1, .before = "BLOCK")
HW1_PreDawn <- add_column(HW1_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
HW1_PreDawn <- add_column(HW1_PreDawn, DayOfHW = "HW", .after = "TimeOfDay")


###Reading midday measurements

#Reading only file from Licor EJF1 Midday
HW1_Midday_EJF1 <- read.csv("CSV/2021-06-17_bh_midday_ejf1.csv")
names(HW1_Midday_EJF1) <- HW1_Midday_EJF1[13,]
HW1_Midday_EJF1 <- HW1_Midday_EJF1[-seq(1,14,1),]

#Reading only file from Licor McElrond Midday
HW1_Midday_Mc  <- read.csv("CSV/2021-06-17-BHmidday_McElrone.csv")
names(HW1_Midday_Mc) <- HW1_Midday_Mc[13,]
HW1_Midday_Mc <- HW1_Midday_Mc[-seq(1,14,1),]


# Merging all dataframes
HW1_Midday <- bind_rows(HW1_Midday_EJF1,HW1_Midday_Mc)

DATE <- as.Date(substr(HW1_Midday$date,1,8), format = "%Y%m%d")
HW1_Midday <- add_column(HW1_Midday, DATE = DATE, .before = "date")

#adding constants
HW1_Midday <- add_column(HW1_Midday, HeatWave = 1, .before = "BLOCK")
HW1_Midday <- add_column(HW1_Midday, TimeOfDay = "Midday", .after = "HeatWave")
HW1_Midday <- add_column(HW1_Midday, DayOfHW = "HW", .after = "TimeOfDay")


## Merging predawn and midday for pre HW 1

HW1 <- bind_rows(HW1_PreDawn,HW1_Midday)



######################################################
################# Post HeatWave 1 ###############################
######################################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/22 June 2021 PostHeatWave')


#Reading only file from Licor EJF1 Predawn
PostHW1_PreDawn_EJF1 <- read.csv("CSV/2021-06-22-Predawn_EJF1.csv")
names(PostHW1_PreDawn_EJF1) <- PostHW1_PreDawn_EJF1[13,]
PostHW1_PreDawn_EJF1 <- PostHW1_PreDawn_EJF1[-seq(1,14,1),]
PostHW1_PreDawn_EJF1 <- PostHW1_PreDawn_EJF1[!apply(PostHW1_PreDawn_EJF1 == "", 1, all), ]

#Reading only file from Licor Annie Predawn
PostHW1_PreDawn_Annie <- read.csv("CSV/2021-06-22-BH_Predawn_Annie.csv")
names(PostHW1_PreDawn_Annie) <- PostHW1_PreDawn_Annie[13,]
PostHW1_PreDawn_Annie <- PostHW1_PreDawn_Annie[-seq(1,14,1),]
PostHW1_PreDawn_Annie <- PostHW1_PreDawn_Annie[!apply(PostHW1_PreDawn_Annie == "", 1, all), ]

# Merging all dataframes
PostHW1_PreDawn <- bind_rows(PostHW1_PreDawn_EJF1,PostHW1_PreDawn_Annie)

DATE <- as.Date(substr(PostHW1_PreDawn$date,1,8), format = "%Y%m%d")
PostHW1_PreDawn <- add_column(PostHW1_PreDawn, DATE = DATE, .before = "date")

#adding constants
PostHW1_PreDawn <- add_column(PostHW1_PreDawn, HeatWave = 1, .before = "BLOCK")
PostHW1_PreDawn <- add_column(PostHW1_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
PostHW1_PreDawn <- add_column(PostHW1_PreDawn, DayOfHW = "PostHW", .after = "TimeOfDay")


#Reading only file from Licor EJF1 Midday
PostHW1_Midday_EJF1 <- read.csv("CSV/2021-06-22-BH_Midday_EJF1.csv")
names(PostHW1_Midday_EJF1) <- PostHW1_Midday_EJF1[13,]
PostHW1_Midday_EJF1 <- PostHW1_Midday_EJF1[-seq(1,14,1),]
PostHW1_Midday_EJF1 <- PostHW1_Midday_EJF1[!apply(PostHW1_Midday_EJF1 == "", 1, all), ]


#Reading only file from Licor Annie Predawn
PostHW1_Midday_Annie <- read.csv("CSV/2021-06-22-BH_Midday_Annie.csv")
names(PostHW1_Midday_Annie) <- PostHW1_Midday_Annie[13,]
PostHW1_Midday_Annie <- PostHW1_Midday_Annie[-seq(1,14,1),]
PostHW1_Midday_Annie <- PostHW1_Midday_Annie[!apply(PostHW1_Midday_Annie == "", 1, all), ]


# Merging all dataframes
PostHW1_Midday <- bind_rows(PostHW1_Midday_EJF1,PostHW1_Midday_Annie)

DATE <- as.Date(substr(PostHW1_Midday$date,1,8), format = "%Y%m%d")
PostHW1_Midday <- add_column(PostHW1_Midday, DATE = DATE, .before = "date")

#adding constants
PostHW1_Midday <- add_column(PostHW1_Midday, HeatWave = 1, .before = "BLOCK")
PostHW1_Midday <- add_column(PostHW1_Midday, TimeOfDay = "Midday", .after = "HeatWave")
PostHW1_Midday <- add_column(PostHW1_Midday, DayOfHW = "PostHW", .after = "TimeOfDay")

Post_HW1 <- bind_rows(PostHW1_PreDawn,PostHW1_Midday)

HW1_All <- bind_rows(Pre_HW1,HW1,Post_HW1)



#####################################
###### Baseline July ###############
#####################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/30 June 2021/')


#Reading only file from Licor Loaner Predawn
BL_July_PreDawn_Annie <- read.csv("CSV/2021-06-30-Bh_predawns_loaner.csv")
names(BL_July_PreDawn_Annie) <- BL_July_PreDawn_Annie[13,]
BL_July_PreDawn_Annie <- BL_July_PreDawn_Annie[-seq(1,14,1),]
BL_July_PreDawn_Annie <- BL_July_PreDawn_Annie[!apply(BL_July_PreDawn_Annie == "", 1, all), ]


#Reading only file from EJF1 Predawn
BL_July_PreDawn_EJF1 <- read.csv("CSV/2021-06-30-Bh_predawns_EJF1.csv")
names(BL_July_PreDawn_EJF1) <- BL_July_PreDawn_EJF1[14,]
BL_July_PreDawn_EJF1 <- BL_July_PreDawn_EJF1[-seq(1,15,1),]


# Merging all dataframes
BaseLineJuly_PreDawn <- bind_rows(BL_July_PreDawn_Annie,BL_July_PreDawn_EJF1)

DATE <- as.Date(substr(BaseLineJuly_PreDawn$date,1,8), format = "%Y%m%d")
BaseLineJuly_PreDawn <- add_column(BaseLineJuly_PreDawn, DATE = DATE, .before = "date")

#adding constants
BaseLineJuly_PreDawn <- add_column(BaseLineJuly_PreDawn, HeatWave = 0, .before = "BLOCK")
BaseLineJuly_PreDawn <- add_column(BaseLineJuly_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
BaseLineJuly_PreDawn <- add_column(BaseLineJuly_PreDawn, DayOfHW = "BaseLine", .after = "TimeOfDay")


###Reading midday measurements

#Reading file 1/2 from Licor Loaner
BL_July_Midday_Annie <- read.csv("CSV/2021-06-30-Bh_Midday_loaner.csv")
names(BL_July_Midday_Annie) <- BL_July_Midday_Annie[13,]
BL_July_Midday_Annie <- BL_July_Midday_Annie[-seq(1,14,1),]

#Reading file 2/2 from Licor Loaner
BL_July_Midday_Annie1 <- read.csv("CSV/2021-06-30-Bh_Midday_loaner1.csv")
names(BL_July_Midday_Annie1) <- BL_July_Midday_Annie1[13,]
BL_July_Midday_Annie1 <- BL_July_Midday_Annie1[-seq(1,14,1),]

BL_July_Midday_Annie_all <-  bind_rows(BL_July_Midday_Annie,BL_July_Midday_Annie1) 

#Reading file 2/2 from Licor Loaner
BL_July_Midday_EJF1 <- read.csv("CSV/2021-06-30-Bh_Midday_EJF1.csv")
names(BL_July_Midday_EJF1) <- BL_July_Midday_EJF1[13,]
BL_July_Midday_EJF1 <- BL_July_Midday_EJF1[-seq(1,14,1),]

# Merging all dataframes
BaseLineJuly_Midday <- bind_rows(BL_July_Midday_Annie_all,BL_July_Midday_EJF1)

DATE <- as.Date(substr(BaseLineJuly_Midday$date,1,8), format = "%Y%m%d")
BaseLineJuly_Midday <- add_column(BaseLineJuly_Midday, DATE = DATE, .before = "date")

#adding constants
BaseLineJuly_Midday <- add_column(BaseLineJuly_Midday, HeatWave = 0, .before = "BLOCK")
BaseLineJuly_Midday <- add_column(BaseLineJuly_Midday, TimeOfDay = "Midday", .after = "HeatWave")
BaseLineJuly_Midday <- add_column(BaseLineJuly_Midday, DayOfHW = "BaseLine", .after = "TimeOfDay")


BaseLineJuly <- bind_rows(BaseLineJuly_PreDawn,BaseLineJuly_Midday)


#####################################
###### Pre Heatwave 2 ###############
#####################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/07 July PreHeatWave')



#Reading only file from Licor Loaner
PreHW2_PreDawn_Loaner <- read.csv("CSV/2021-07-07_BH_Predawn_Loaner.csv")
names(PreHW2_PreDawn_Loaner) <- PreHW2_PreDawn_Loaner[14,]
PreHW2_PreDawn_Loaner <- PreHW2_PreDawn_Loaner[-seq(1,15,1),]

#Reading only file from EJF1 Predawn
PreHW2_PreDawn_EJF1 <- read.csv("CSV/2021-07-07_BH_Predawn_EJF1.csv")
names(PreHW2_PreDawn_EJF1) <- PreHW2_PreDawn_EJF1[14,]
PreHW2_PreDawn_EJF1 <- PreHW2_PreDawn_EJF1[-seq(1,15,1),]


# Merging all dataframes
Pre_HW2_PreDawn <- bind_rows(PreHW2_PreDawn_Loaner,PreHW2_PreDawn_EJF1)

DATE <- as.Date(substr(Pre_HW2_PreDawn$date,1,8), format = "%Y%m%d")
Pre_HW2_PreDawn <- add_column(Pre_HW2_PreDawn, DATE = DATE, .before = "date")

#adding constants
Pre_HW2_PreDawn <- add_column(Pre_HW2_PreDawn, HeatWave = 2, .before = "BLOCK")
Pre_HW2_PreDawn <- add_column(Pre_HW2_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
Pre_HW2_PreDawn <- add_column(Pre_HW2_PreDawn, DayOfHW = "PreHW", .after = "TimeOfDay")


###Reading midday measurements

#Reading only file from Licor EJF1 Midday
PreHW2_Midday_EJF1 <- read.csv("CSV/2021-07-07_BH_Midday_EJF1.csv")
names(PreHW2_Midday_EJF1) <- PreHW2_Midday_EJF1[14,]
PreHW2_Midday_EJF1 <- PreHW2_Midday_EJF1[-seq(1,15,1),]

#Reading only file from Licor McElrond Midday
PreHW2_Midday_Loaner  <- read.csv("CSV/2021-07-07_BH_Midday_Loaner.csv")
names(PreHW2_Midday_Loaner) <- PreHW2_Midday_Loaner[14,]
PreHW2_Midday_Loaner <- PreHW2_Midday_Loaner[-seq(1,15,1),]


# Merging all dataframes
Pre_HW2_MidDay <- bind_rows(PreHW2_Midday_EJF1,PreHW2_Midday_Loaner)

DATE <- as.Date(substr(Pre_HW2_MidDay$date,1,8), format = "%Y%m%d")
Pre_HW2_MidDay <- add_column(Pre_HW2_MidDay, DATE = DATE, .before = "date")

#adding constants
Pre_HW2_MidDay <- add_column(Pre_HW2_MidDay, HeatWave = 2, .before = "BLOCK")
Pre_HW2_MidDay <- add_column(Pre_HW2_MidDay, TimeOfDay = "Midday", .after = "HeatWave")
Pre_HW2_MidDay <- add_column(Pre_HW2_MidDay, DayOfHW = "PreHW", .after = "TimeOfDay")

## Merging predawn and midday for pre HW 1

Pre_HW2 <- bind_rows(Pre_HW2_PreDawn,Pre_HW2_MidDay)




######################################################
################# HW2 ###############################
######################################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/10 July Heatwave')


#Reading only file from Licor EJF1 Predawn
HW2_PreDawn_EJF1 <- read.csv("CSV/2021-07-10_BH_predawn_EJF1.csv")
names(HW2_PreDawn_EJF1) <- HW2_PreDawn_EJF1[14,]
HW2_PreDawn_EJF1 <- HW2_PreDawn_EJF1[-seq(1,15,1),]


#Reading only file from Loaner Predawn
HW2_PreDawn_Loaner <- read.csv("CSV/2021-07-10_BH_predawn_Loaner.csv")
names(HW2_PreDawn_Loaner) <- HW2_PreDawn_Loaner[14,]
HW2_PreDawn_Loaner <- HW2_PreDawn_Loaner[-seq(1,15,1),]
#deleting rows with empty cells
HW2_PreDawn_Loaner <- HW2_PreDawn_Loaner[!apply(HW2_PreDawn_Loaner == "", 1, all), ]  

# Merging all dataframes
HW2_PreDawn <- bind_rows(HW2_PreDawn_EJF1,HW2_PreDawn_Loaner)

DATE <- as.Date(substr(HW2_PreDawn$date,1,8), format = "%Y%m%d")
HW2_PreDawn <- add_column(HW2_PreDawn, DATE = DATE, .before = "date")

#adding constants
HW2_PreDawn <- add_column(HW2_PreDawn, HeatWave = 2, .before = "BLOCK")
HW2_PreDawn <- add_column(HW2_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
HW2_PreDawn <- add_column(HW2_PreDawn, DayOfHW = "HW", .after = "TimeOfDay")


###Reading midday measurements

#Reading only file from Licor EJF1 Midday
HW2_Midday_EJF1 <- read.csv("CSV/2021-07-10_BH_Midday_EJF1.csv")
names(HW2_Midday_EJF1) <- HW2_Midday_EJF1[14,]
HW2_Midday_EJF1 <- HW2_Midday_EJF1[-seq(1,15,1),]

#Reading only file from Licor McElrond Midday
HW2_Midday_Mc  <- read.csv("CSV/2021-07-10_BH_Midday_McElrone.csv")
names(HW2_Midday_Mc) <- HW2_Midday_Mc[14,]
HW2_Midday_Mc <- HW2_Midday_Mc[-seq(1,15,1),]


# Merging all dataframes
HW2_Midday <- bind_rows(HW2_Midday_EJF1,HW2_Midday_Mc)

DATE <- as.Date(substr(HW2_Midday$date,1,8), format = "%Y%m%d")
HW2_Midday <- add_column(HW2_Midday, DATE = DATE, .before = "date")

#adding constants
HW2_Midday <- add_column(HW2_Midday, HeatWave = 2, .before = "BLOCK")
HW2_Midday <- add_column(HW2_Midday, TimeOfDay = "Midday", .after = "HeatWave")
HW2_Midday <- add_column(HW2_Midday, DayOfHW = "HW", .after = "TimeOfDay")


## Merging predawn and midday for pre HW 1

HW2 <- bind_rows(HW2_PreDawn,HW2_Midday)



######################################################
################# Post HeatWave 2 ###############################
######################################################

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/14 July PostHeatWave')


#Reading only file from Licor EJF1 Predawn
PostHW2_PreDawn_EJF1 <- read.csv("CSV/2021-07-14-BH_Predawn_EJF1.csv")
names(PostHW2_PreDawn_EJF1) <- PostHW2_PreDawn_EJF1[14,]
PostHW2_PreDawn_EJF1 <- PostHW2_PreDawn_EJF1[-seq(1,15,1),]
PostHW2_PreDawn_EJF1 <- PostHW2_PreDawn_EJF1[!apply(PostHW2_PreDawn_EJF1 == "", 1, all), ]

#Reading only file from Licor McElrond
PostHW2_PreDawn_Mc <- read.csv("CSV/2021-07-14-BH_Predawn_Mc.csv")
names(PostHW2_PreDawn_Mc) <- PostHW2_PreDawn_Mc[14,]
PostHW2_PreDawn_Mc <- PostHW2_PreDawn_Mc[-seq(1,15,1),]
#PostHW1_PreDawn_Annie <- PostHW1_PreDawn_Annie[!apply(PostHW1_PreDawn_Annie == "", 1, all), ]

# Merging all dataframes
PostHW2_PreDawn <- bind_rows(PostHW2_PreDawn_EJF1,PostHW2_PreDawn_Mc)

DATE <- as.Date(substr(PostHW2_PreDawn$date,1,8), format = "%Y%m%d")
PostHW2_PreDawn <- add_column(PostHW2_PreDawn, DATE = DATE, .before = "date")

#adding constants
PostHW2_PreDawn <- add_column(PostHW2_PreDawn, HeatWave = 2, .before = "BLOCK")
PostHW2_PreDawn <- add_column(PostHW2_PreDawn, TimeOfDay = "Predawn", .after = "HeatWave")
PostHW2_PreDawn <- add_column(PostHW2_PreDawn, DayOfHW = "PostHW", .after = "TimeOfDay")


#Reading only file from Licor EJF1 Midday
PostHW2_Midday_EJF1 <- read.csv("CSV/2021-07-14-BH_Midday_EJF1.csv")
names(PostHW2_Midday_EJF1) <- PostHW2_Midday_EJF1[14,]
PostHW2_Midday_EJF1 <- PostHW2_Midday_EJF1[-seq(1,15,1),]
#PostHW2_Midday_EJF1 <- PostHW1_Midday_EJF1[!apply(PostHW1_Midday_EJF1 == "", 1, all), ]


#Reading only file from Licor McElrond
PostHW2_Midday_Mc <- read.csv("CSV/2021-07-14-BH_Midday_Mc.csv")
names(PostHW2_Midday_Mc) <- PostHW2_Midday_Mc[14,]
PostHW2_Midday_Mc <- PostHW2_Midday_Mc[-seq(1,15,1),]
#PostHW1_Midday_Annie <- PostHW1_Midday_Annie[!apply(PostHW1_Midday_Annie == "", 1, all), ]


# Merging all dataframes
PostHW2_Midday <- bind_rows(PostHW2_Midday_EJF1,PostHW2_Midday_Mc)

DATE <- as.Date(substr(PostHW2_Midday$date,1,8), format = "%Y%m%d")
PostHW2_Midday <- add_column(PostHW2_Midday, DATE = DATE, .before = "date")

#adding constants
PostHW2_Midday <- add_column(PostHW2_Midday, HeatWave = 2, .before = "BLOCK")
PostHW2_Midday <- add_column(PostHW2_Midday, TimeOfDay = "Midday", .after = "HeatWave")
PostHW2_Midday <- add_column(PostHW2_Midday, DayOfHW = "PostHW", .after = "TimeOfDay")

Post_HW2 <- bind_rows(PostHW2_PreDawn,PostHW2_Midday)

HW2_All <- bind_rows(Pre_HW2,HW2,Post_HW2)



#############################################
#### Baseline Late July Measurements ########
#############################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/29 July 2021")

#Reading only file from Loaner Licor midday
BL_LateJuly_Midday_Annie <- read.csv("CSV/2021-07-28-BH_middays_Loaner.csv")
names(BL_LateJuly_Midday_Annie) <- BL_LateJuly_Midday_Annie[13,]
BL_LateJuly_Midday_Annie <- BL_LateJuly_Midday_Annie[-seq(1,14,1),]

#Reading only file from EJF1 midday
BL_LateJuly_Midday_EJF1 <- read.csv("CSV/2021-07-28-BH_middays_EJF1.csv")
names(BL_LateJuly_Midday_EJF1) <- BL_LateJuly_Midday_EJF1[13,]
BL_LateJuly_Midday_EJF1 <- BL_LateJuly_Midday_EJF1[-seq(1,14,1),]

# Merging all dataframes
BaseLineLateJuly_Midday <- bind_rows(BL_LateJuly_Midday_Annie,BL_LateJuly_Midday_EJF1)

DATE <- as.Date(substr(BaseLineLateJuly_Midday$date,1,8), format = "%Y%m%d")
BaseLineLateJuly_Midday <- add_column(BaseLineLateJuly_Midday, DATE = DATE, .before = "date")


#adding constants
BaseLineLateJuly_Midday <- add_column(BaseLineLateJuly_Midday, HeatWave = 0, .before = "BLOCK")
BaseLineLateJuly_Midday <- add_column(BaseLineLateJuly_Midday, TimeOfDay = "Midday", .after = "HeatWave")
BaseLineLateJuly_Midday <- add_column(BaseLineLateJuly_Midday, DayOfHW = "BaseLine", .after = "TimeOfDay")



#############################################
#### Baseline August Measurements ########
#############################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/11 August 2021")

#Reading only file from Licor EJF1 midday
BL_August_Midday_EJF1 <- read.csv("CSV/2021-08-11-BH_Midday_EJF1.csv")
names(BL_August_Midday_EJF1) <- BL_August_Midday_EJF1[13,]
BL_August_Midday_EJF1 <- BL_August_Midday_EJF1[-seq(1,14,1),]

#Reading only file from McElrone midday
BL_August_Midday_Mc <- read.csv("CSV/2021-08-11-BH_Midday_Mc.csv")
names(BL_August_Midday_Mc) <- BL_August_Midday_Mc[13,]
BL_August_Midday_Mc <- BL_August_Midday_Mc[-seq(1,14,1),]

# Merging all dataframes
BaseLineAugust_Midday <- bind_rows(BL_August_Midday_EJF1,BL_August_Midday_Mc)

#The Licors don't have the same date but obviously measurements were taken the same day
DATE <- as.Date("2021-08-11")
BaseLineAugust_Midday <- add_column(BaseLineAugust_Midday, DATE = DATE, .before = "date")


#adding constants
BaseLineAugust_Midday <- add_column(BaseLineAugust_Midday, HeatWave = 0, .before = "BLOCK")
BaseLineAugust_Midday <- add_column(BaseLineAugust_Midday, TimeOfDay = "Midday", .after = "HeatWave")
BaseLineAugust_Midday <- add_column(BaseLineAugust_Midday, DayOfHW = "BaseLine", .after = "TimeOfDay")



#############################################
#### Baseline September Measurements ########
#############################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/30 August 2021")

#Reading only file from Licor EJF1 predawn
BL_September_Predawn_EJF1 <- read.csv("CSV/2021-08-30-predawns_EJF1.csv")
names(BL_September_Predawn_EJF1) <- BL_September_Predawn_EJF1[13,]
BL_September_Predawn_EJF1 <- BL_September_Predawn_EJF1[-seq(1,14,1),]

#Reading only file from Licor EJF2 predawn
BL_September_Predawn_EJF2 <- read.csv("CSV/2021-08-30-predawns_EJF2.csv")
names(BL_September_Predawn_EJF2) <- BL_September_Predawn_EJF2[15,]
BL_September_Predawn_EJF2 <- BL_September_Predawn_EJF2[-seq(1,16,1),]

# Merging all dataframes
BaseLineSeptember_Predawn <- bind_rows(BL_September_Predawn_EJF1,BL_September_Predawn_EJF2)

DATE <- as.Date(substr(BaseLineSeptember_Predawn$date,1,8), format = "%Y%m%d")
BaseLineSeptember_Predawn <- add_column(BaseLineSeptember_Predawn, DATE = DATE, .before = "date")


#adding constants
BaseLineSeptember_Predawn <- add_column(BaseLineSeptember_Predawn, HeatWave = 0, .before = "BLOCK")
BaseLineSeptember_Predawn <- add_column(BaseLineSeptember_Predawn, TimeOfDay = "Predawn", .after = "HeatWave")
BaseLineSeptember_Predawn <- add_column(BaseLineSeptember_Predawn, DayOfHW = "BaseLine", .after = "TimeOfDay")


###Reading midday measurements

#Reading only file from Licor EJF1 Midday
BL_September_Midday_EJF1 <- read.csv("CSV/2021-08-30-Midday_EJF1.csv")
names(BL_September_Midday_EJF1) <- BL_September_Midday_EJF1[13,]
BL_September_Midday_EJF1 <- BL_September_Midday_EJF1[-seq(1,14,1),]

#Reading only file from Licor EJF2 Midday
BL_September_Midday_EJF2  <- read.csv("CSV/2021-08-30-Midday_EJF2.csv")
names(BL_September_Midday_EJF2) <- BL_September_Midday_EJF2[15,]
BL_September_Midday_EJF2 <- BL_September_Midday_EJF2[-seq(1,16,1),]


# Merging all dataframes
BL_September <- bind_rows(BL_September_Midday_EJF1,BL_September_Midday_EJF2)

DATE <- as.Date(substr(BL_September$date,1,8), format = "%Y%m%d")
BL_September <- add_column(BL_September, DATE = DATE, .before = "date")

#adding constants
BL_September <- add_column(BL_September, HeatWave = 0, .before = "BLOCK")
BL_September <- add_column(BL_September, TimeOfDay = "Midday", .after = "HeatWave")
BL_September <- add_column(BL_September, DayOfHW = "BaseLine", .after = "TimeOfDay")


## Merging predawn and midday for pre HW 1

BL_September <- bind_rows(BaseLineSeptember_Predawn,BL_September)


#############################################
#################### HW3 ########################
#############################################


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/Physiology/Li-Cor Data/08 September 2021 Heatwave")

#Reading only file from Licor EJF1 Midday
HW3_Midday_EJF1 <- read.csv("CSV/2021-09-08_bh_midday_EJF1.csv")
names(HW3_Midday_EJF1) <- HW3_Midday_EJF1[13,]
HW3_Midday_EJF1 <- HW3_Midday_EJF1[-seq(1,14,1),]

#Reading only file from Licor EJF2 Midday
HW3_Midday_EJF2 <- read.csv("CSV/2021-09-08-BH_Midday_EJF2.csv")
names(HW3_Midday_EJF2) <- HW3_Midday_EJF2[15,]
HW3_Midday_EJF2 <- HW3_Midday_EJF2[-seq(1,16,1),]

# Merging all dataframes
HW3 <- bind_rows(HW3_Midday_EJF1,HW3_Midday_EJF2)

DATE <- as.Date(substr(HW3$date,1,8), format = "%Y%m%d")
HW3 <- add_column(HW3, DATE = DATE, .before = "date")


#adding constants
HW3 <- add_column(HW3, HeatWave = 3, .before = "BLOCK")
HW3 <- add_column(HW3, TimeOfDay = "Midday", .after = "HeatWave")
HW3 <- add_column(HW3, DayOfHW = "HW", .after = "TimeOfDay")


##########################################
########## Merging All !!! ###############
##########################################

Licor_All <- bind_rows(BaseLineJune_Midday,HW1_All,BaseLineJuly,HW2_All,BaseLineLateJuly_Midday,
                       BaseLineAugust_Midday,BL_September,HW3)

Licor_All <- Licor_All %>% mutate(Treatment = substr(BLOCK,1,2), .before = BLOCK)
Licor_All$Treatment <- factor(Licor_All$Treatment, levels = c("B1","B2","B3")) 

#Cleaning Photosynthesis
Licor_All$A <- as.numeric(Licor_All$A)
hist(Licor_All$A)
Licor_All <- Licor_All %>% filter(A > -5) #weird values
min(Licor_All$A)

#Cleaning Stomatal Conductance
Licor_All$gsw <- as.numeric(Licor_All$gsw)
Licor_All <- Licor_All %>% filter(gsw < 2) #weird values
hist(Licor_All$gsw)

Licor_All$E <- as.numeric(Licor_All$E)
Licor_All <- Licor_All %>% filter(E < 0.015) #weird values
hist(Licor_All$E)


write.csv(Licor_All,"/Volumes/GoogleDrive/.shortcut-targets-by-id/1hyQpmjew1d2tqLaOtVkmoRuEyMTqmVzL/Lodi_2021/BordenStations_2021/BordenHills2021_Complete/Licor_All.csv",
          row.names = FALSE, col.names = TRUE)


ggplot(data = Licor_All, aes(x = DATE, y = gsw)) +
  geom_boxplot(aes(fill = Treatment), width = 0.6)


ggplot(data = Licor_All %>% filter(TimeOfDay == "Midday"), aes(x = DATE, y = A)) +
  geom_boxplot(aes(fill = Treatment,
                   group = interaction(factor(DATE), Treatment)),  width =2.5, position=position_dodge(3), outlier.size=0.75) + 
  ylab(bquote("An ("*mu~"mol" ~CO[2]~ m^-2~s^-1*")")) +
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

ggsave('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Physiology/AssimilationSeason.pdf')





ggplot(data = Licor_All %>% filter(TimeOfDay == "Midday"), aes(x = DATE, y = gsw)) +
  geom_boxplot(aes(fill = Treatment,
                   group = interaction(factor(DATE), Treatment)),  width =2.5, position=position_dodge(3), outlier.size=0.75) + 
  ylab(bquote("gs (mol" ~H[2]*"O"~ m^-2 ~s^-1*")")) +
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



ggsave('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Physiology/StomatalConSeason.pdf')





