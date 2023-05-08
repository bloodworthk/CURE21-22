#### Master Script for CURE 21-22 Manuscript ####
#Alyssa Young Script


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(tidyverse)
#set colorblind friendly color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Set working directory ####
setwd("C:/Users/alyou/Box/SIDE PROJECTS/CURE21-22/Data")

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
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_")

#read in weekly data
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", colClasses = c("character", "character", "character", "character","character", "factor", "factor", "factor", "factor", "factor", "factor", "factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-fall_treatment,-spring_treatment)

#read in end timepoint ANPP & BNPP Measurements 
ANPP_BNPP <- read.csv("spring2022_ANPP_BNPP.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "character")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") 

#read in end timepoint leaf metrics
Leaf_Data <- read.csv("spring2022_llp315cure_Leafcombo.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") 
  

#### Clean Up Data Through Time Data ####

#correcting data entry for plant ID so that it is the same as data in "PlantID" dataframe
#fall plant ID column
Through_Time$fall_plant_ID<-gsub("P00","P", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("P0","P", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("WEd","Wed", Through_Time$fall_plant_ID)
Through_Time$fall_plant_ID<-gsub("-","_", Through_Time$fall_plant_ID)
#spring plant ID column
Through_Time$spring_plant_ID<-gsub(" - ","_", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("-","_", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("MON","Mon", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Monn","Mon", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("THUR","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Thu","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Thursrs","Thurs", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Tue","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("Tuess","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("TUES","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("TUE","Tues", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub("WED","Wed", Through_Time$spring_plant_ID)
Through_Time$spring_plant_ID<-gsub(" ","", Through_Time$spring_plant_ID)

#Make two dataframes for spring and fall and join PlantID
Through_Time_Fall<-Through_Time %>% 
  filter(fall_plant_ID!="NA") %>%
  select(-spring_plant_ID) %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  full_join(PlantID) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(fall_plant_ID=paste(fall_day,fall_treatment,fall_plant,sep="_"))

Through_Time_Spring<-Through_Time %>% 
  filter(spring_plant_ID!="NA") %>% 
  select(-fall_plant_ID) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(fall_plant_ID=paste(fall_day,fall_treatment,fall_plant,sep="_"))

#join Through_Time with PlantID
Through_Time_Join<-Through_Time_Fall %>% 
  rbind(Through_Time_Spring) %>% 
  mutate(max_leaf_length=ifelse(survival=="D",NA,max_leaf_length)) %>% 
  mutate(max_plant_height=ifelse(survival=="D",NA,max_plant_height)) %>% 
  mutate(leaf_num=ifelse(survival=="D",NA,leaf_num))
  

Through_Time_Final<-Through_Time_Join %>% 
  select(c(overall_group,fall_plant_ID,spring_plant_ID,date,week_num,tray_ID,survival,
           max_leaf_length,max_plant_height,leaf_num,soil_moisture,light_avail,
           air_temp,humidity))#### some things have NAs here -- look at data to find problem ####
#### Clean Up Week 22 Data ####

End_Time_Point<-Through_Time_Join %>% 
  filter(week_num=="22") %>% 
  select(overall_group,spring_plant_ID,fall_plant_ID,date,week_num,tray_ID,survival,plant_stress,leaf_num) ###currently missing data #####

#### Clean Up EndPoint Data ####

#join ANPP_BNPP dataframe with plant
NPP_Join <- PlantID %>%
  select(-fall_day,-fall_treatment,-fall_plant,-fall_pot_num) %>% 
  #join Plant data
  full_join(ANPP_BNPP) %>%
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  #compute mutate step row by row
  rowwise() %>% 
  #total up the total NPP
  mutate(NPP = sum(c(total_ANPP_g, BNPP_g))) %>% 
  select(overall_group,spring_plant_ID,alive_ANPP_g,dead_ANPP_g,total_ANPP_g,BNPP_g,NPP,comments,notes) #NAs -- not sure why?

#join leaf data with plantID
Leaf_Data_Join <- Leaf_Data %>%
  full_join(PlantID) %>%
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  select(overall_group,spring_plant_ID,leaf_number,wet_leaf_weight,dry_leaf_weight,leaf_area,leaf_thickness) %>% 
  mutate(LDMC = dry_leaf_weight / wet_leaf_weight) %>%
  mutate(SLA = leaf_area / dry_leaf_weight) 

#####################################################################
# Relative Growth Rate - Leaf Number #
Through_Time_Final2 <- Through_Time_Final %>%
  drop_na(leaf_num) %>%
  drop_na(overall_group) %>%
  drop_na(week_num) %>%
  group_by(overall_group, spring_plant_ID) %>%
  unique() %>%
  filter(spring_plant_ID != "Wed_SC_125" | leaf_num != 34) %>% #removing incorrectly entered columns
  filter(spring_plant_ID != "Wed_SH_177" | leaf_num != 22) %>% #removing incorrectly entered columns
  filter(spring_plant_ID != "Wed_SH_165" | max_plant_height != 79) #removing incorrectly entered columns

# change incorrect leaf number value #
Through_Time_Final2[1868,10]<-46

# Week 1-2 slope #
leafnum_week1 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "1") %>%
  rename(leafnum_wk1 = leaf_num) %>%
  rename(week_1 = week_num)
leafnum_week2 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "2") %>%
  rename(leafnum_wk2 = leaf_num) %>%
  rename(week_2 = week_num)
leafnum_week1_2 <- leafnum_week1 %>%
  full_join(leafnum_week2) %>%
  mutate(slope = (leafnum_wk2 - leafnum_wk1)/1)

leafnum_W1_2 <- leafnum_week1_2 %>%
  mutate(overall_group = ifelse(overall_group=="Control-Control", "Control", 
              ifelse(overall_group=="Control-Heatwave", "Control",
              ifelse(overall_group=="Heatwave-Control", "Heatwave",
              ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W1-2") %>%
  select(overall_group, spring_plant_ID, slope, timepoint)

  
# Week 3-5 slope #
leafnum_week3 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "3") %>%
  rename(leafnum_wk3 = leaf_num) %>%
  rename(week_3 = week_num)
leafnum_week5 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "5") %>%
  rename(leafnum_wk5 = leaf_num) %>%
  rename(week_5 = week_num)
leafnum_week3_5 <- leafnum_week3 %>%
  full_join(leafnum_week5) %>%
  mutate(slope = (leafnum_wk5 - leafnum_wk3)/2)


leafnum_W3_5 <- leafnum_week3_5 %>%
  mutate(overall_group = ifelse(overall_group=="Control-Control", "Control", 
                        ifelse(overall_group=="Control-Heatwave", "Control",
                        ifelse(overall_group=="Heatwave-Control", "Heatwave",
                       ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W3-5") %>%
  select(overall_group, spring_plant_ID, slope, timepoint)


# Week 9-18 slope #
leafnum_week9 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "9") %>%
  rename(leafnum_wk9 = leaf_num) %>%
  rename(week_9 = week_num)
leafnum_week18 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "18") %>%
  rename(leafnum_wk18 = leaf_num) %>%
  rename(week_18 = week_num)
leafnum_W9_18 <- leafnum_week9 %>%
  full_join(leafnum_week18) %>%
  mutate(slope = (leafnum_wk18 - leafnum_wk9)/9) %>%
  add_column(timepoint = "W9-18") %>%
  select(overall_group, spring_plant_ID, slope, timepoint)



# Week 19-22 slope #
leafnum_week19 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "19") %>%
  rename(leafnum_wk19 = leaf_num) %>%
  rename(week_19 = week_num)
leafnum_week22 <- Through_Time_Final2 %>%
  dplyr::select(-c(date, tray_ID, survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity)) %>%
  filter(week_num == "22") %>%
  rename(leafnum_wk22 = leaf_num) %>%
  rename(week_22 = week_num)
leafnum_W19_22 <- leafnum_week19 %>%
  full_join(leafnum_week22) %>%
  mutate(slope = (leafnum_wk22 - leafnum_wk19)/3) %>%
  add_column(timepoint = "W19-22") %>%
  select(overall_group, spring_plant_ID, slope, timepoint)


# merge all data frames together #
finalLeafNumSlope <- leafnum_W1_2 %>% 
  rbind(leafnum_W3_5) %>%
  rbind(leafnum_W9_18) %>%
  rbind(leafnum_W19_22)
  



ggplot(finalLeafNumSlope,aes(x=factor(timepoint, level=c('W1-2', 'W3-5', 'W9-18', 'W19-22')),y=slope, fill=overall_group))+
  geom_boxplot()
