Scripts to extract, compile, clean and average datalogger data from BordenHill 2020

The order of the scripts and description is as follow:

1)Compiling_Met_Thermo_Files.R --> extracts the tables (Met, IRT, BB and Thermocouples) from Google Drive and compile the whole timeseries in one single Rdata for each one of the tables. Data is sorted and timestamps redundancies are eliminated. Line 13 sets the working directory for the raw data (each extraction from the towers).
Line 14 sets the directory for the outcome tables.

2)Thermocouples_script_v3.R -->  First half of the script reads only the thermocouple outputs from "Compiling_Met_Thermo_Files.R" , averages the temperatures at 15 min intervals, deletes values over 70 C and below 0 C.

The second half of the script goes stations by station in order to further filter temperatures measurements. Temperatures that are over x +/- standard deviations from a given time period (every 15 min) are converted into NA. Threshold can be changed by adjusting Cut variable.

The, field notes are matched with the observations (e.g, if a leaf having thermocouple burns, a berry shrivels, or one thermocouple comes out from a berry or leaf). Comments regarding these observations are annotated in the script.
Here are again

B1R1
#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13

#Berry Thermocouple 7 was found out of the berry and changed the 31/07 to another one,
#according to visual observation of the graph it might have come out the 27/07

#Berry Thermocouple 1 was found wilted and changed the 31/07 to another one,
#according to visual observation of the graph it might have come out the 27/07

#Berry Thermocouple 1 was found wilted and changed the 20/08 to another one,
#according to visual observation of the graph it might have come out the 20/08

B1R4

#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; then TC 17 was rescued on 07/31


B1R3
#Leaves where Thermocouples were placed in the North (channels 13; 14; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

B2R1
#Leaves where Thermocouples were placed in the North (channels 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

#Leaf from TC 19 was found completely burnt the 08/20, 
#according to visual examination of the graph it burnt the 9th of August

#Berries from TC 4 and 12 were found shrivelled the 08/09, 
#according to visual examination of the graph the shivelled the 3rd Spetember

B2R2
#Leaves where Thermocouples were placed in the North (channels 13;17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

#Leaf from TC 19 was found completely burnt the 08/20, 
#according to visual examination of the graph it burnt the 9th of August

#Berries from TC 4 and 12 were found shrivelled the 08/09, 
#according to visual examination of the graph the shivelled the 3rd Spetember


B2R3
#Leaves where Thermocouples were placed in the North (channels 13; 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

#TC 4 was found outside its berry the 08/06 and put again in a berry
#Remove all measurements prior that date


B3R1
#Leaves where Thermocouples were placed in the North (channels 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

#TC 11 (berry) showed erratic behaviour, was deleted from the dataset


B3R2

#Leaves where Thermocouples were placed in the North (channels 14, 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 

B3R3

#Leaves where Thermocouples were placed in the North (channels 13, 17; 18) Side of the canopy
#Didn't survive leaf removal on 06-13; 


#TC 12 (Berry) broke 08/10; no need to delete data, when broken output <- NA





2)Thermocouples_script_v3.R --> averages thermocouples every 15 min and rough clean 3)IRT_Script --> averages IRT and air tempeerature every 15 min 4)Compiling_All_One_Table.R --> grabs all the 15 min averages and compile them in one single table per timestamp/site




'/content/drive/My Drive/Lodi_Heatwave/Data/Lodi_2021/BordenStations_2021/Outputs_2021/D18/'