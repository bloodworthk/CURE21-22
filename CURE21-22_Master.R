#### Master Script for CURE 21-22 Manuscript ####


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(tidyverse)
#set colorblind friendly color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Set working directory ####
#Bloodworth:mac
setwd("/Users/kathrynbloodworth/Library/CloudStorage/Box-Box/Projects/CURE_2021-2022/Data")

#### Update ggplot2 theme ####
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, 
#Place a margin of 15 around the x-axis title.  
#Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  
#Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  
#Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=50, vjust=-0.35, margin=margin(t=12)),
             axis.text.x=element_text(size=50), axis.title.y=element_text(size=50, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=50), plot.title =
               element_text(size=50, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             legend.text=element_text(size=50))

#### Read in Data ####

#read in plant ID meta-data
PlantID <- read.csv("plant_IDs.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "factor", "character", "factor", "factor", "factor", "factor")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  #remove fall P185, P190, P193 because they only contained crabgrass throughout experiment
  filter(fall_plant!="P185" & fall_plant!="P190" & fall_plant!="P193")

#read in biomass removed data
Biomass_Removed <- read.csv("removed_biomass.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "factor")) %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  select(fall_treatment, fall_day, fall_plant, overall_group, biomass_removed)

#read in weekly data
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", colClasses = c("character", "character", "character", "character","character", "numeric", "factor", "factor", "factor", "factor", "factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-c(student_name,start_time,end_time))

#read in end timepoint ANPP & BNPP Measurements 
ANPP_BNPP <- read.csv("spring2022_ANPP_BNPP.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "character")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") 

#read in end timepoint leaf metrics
Leaf_Data <- read.csv("spring2022_llp315cure_Leafcombo.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") 


#### Clean Up Biomass Removed Data and add to Plant_ID####
Biomass_Removed$fall_treatment <- gsub(" ","",Biomass_Removed$fall_treatment)
Biomass_Removed$fall_day <- gsub(" ","",Biomass_Removed$fall_day)
Biomass_Removed$fall_plant <- gsub("p","P",Biomass_Removed$fall_plant)
Biomass_Removed$fall_plant <- gsub(" ","",Biomass_Removed$fall_plant)

PlantID_BiomassRemoved<-PlantID %>% 
  full_join(Biomass_Removed)
#replace NAs with zero for biomass_removed
PlantID_BiomassRemoved$biomass_removed[is.na(PlantID_BiomassRemoved$biomass_removed)] <- 0


#### Clean Up Data Through Time Data ####

#correcting data entry for plant ID so that it is the same as data in "PlantID" dataframe
#fall plant ID column
Through_Time$fall_plant_ID<-gsub("P00","P", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("P0","P", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("WEd","Wed", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("-","_", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("mon","Mon", Through_Time$fall_plant_ID)
#spring plant ID column
Through_Time$spring_plant_ID<-gsub(" - ","_", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("-","_", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Thu","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("THUR","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Thursrs","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Tue","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Tuess","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("TUES","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("TUE","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub(" ","", Through_Time$spring_plant_ID)

#Make two dataframes for spring and fall and join PlantID to each then bind back togehter
Through_Time_Fall<-Through_Time %>% 
  filter(fall_plant_ID!="NA") %>%
  #remove plants that grew only crabgrass from all weeks
  select(-spring_plant_ID) %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  filter(fall_plant!="P185" & fall_plant!="P190" & fall_plant!="P193") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  select(overall_group, week_num,spring_plant_ID,survival,max_leaf_length,max_plant_height,leaf_num,plant_stress,soil_moisture,light_avail,air_temp,humidity,biomass_removed)

Through_Time_Spring<-Through_Time %>% 
  filter(spring_plant_ID!="NA") %>% 
  select(-fall_plant_ID) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  select(overall_group, week_num,spring_plant_ID,survival,max_leaf_length,max_plant_height,leaf_num,plant_stress,soil_moisture,light_avail,air_temp,humidity,biomass_removed)

#join fall and spring through time
Through_Time_Join<-Through_Time_Fall %>% 
  rbind(Through_Time_Spring) 
#remove rows before week 9 that have biomass removal in them
Through_Time_Join = Through_Time_Join[!(Through_Time_Join$week_num < 9 & Through_Time_Join$biomass_removed > 0), ]


#### Clean Up EndPoint Data ####

# Create Week 22 Data

End_Time_Point <- Through_Time_Join %>% 
  filter(week_num=="22") %>% 
  select(-c(week_num,soil_moisture,light_avail,air_temp,humidity))

#join ANPP_BNPP dataframe with plant
NPP_Join <- ANPP_BNPP %>%
  #join Plant data
  full_join(PlantID_BiomassRemoved) %>%
  #remove any plants with NAs for Alive ANPP because they didn't get measured
  drop_na(alive_ANPP_g) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  #compute mutate step row by row
  rowwise() %>% 
  #total up the total NPP
  mutate(NPP = sum(c(total_ANPP_g, BNPP_g))) %>% 
  select(overall_group,spring_plant_ID,alive_ANPP_g,dead_ANPP_g,total_ANPP_g,BNPP_g,NPP,biomass_removed,comments) 

#join leaf data with plantID
Leaf_Data_Join <- Leaf_Data %>%
  #remove plants with no leaf data
  drop_na(wet_leaf_weight) %>% 
  full_join(PlantID_BiomassRemoved) %>%
  drop_na(leaf_number) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(LDMC = dry_leaf_weight / wet_leaf_weight) %>%
  mutate(SLA = leaf_area / dry_leaf_weight) %>% 
  select(overall_group,spring_plant_ID,leaf_number,wet_leaf_weight,dry_leaf_weight,leaf_area,leaf_thickness,biomass_removed)

