#### Master Script for CURE 21-22 Manuscript ####
#Alyssa Young Script


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(lmerTest)
library(stringr)  
library(multcomp)
library(tidyverse)
library(olsrr)


#### Set working directory ####
setwd("C:/Users/alyou/Box/SIDE PROJECTS/CURE21-22/Data")

#### Update ggplot2 theme ####
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, 
#Place a margin of 15 around the x-axis title.  
#Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  
#Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  
#Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=50, vjust=-0.35, margin=margin(t=12)),
             axis.text.x=element_text(size=50),
             axis.title.y=element_text(size=50, angle=90, vjust=0.5, margin=margin(r=15)),
             axis.text.y=element_text(size=50),
             plot.title =element_blank(),
             legend.position = "none",
             legend.text=element_text(size=50),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             axis.line = element_line(colour = "black")
)


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
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", 
colClasses = c("character", "character", "character", "character","character", "numeric", "factor", 
               "factor", "factor", "factor", "factor","numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-c(ï..student_name,start_time,end_time))

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

#for models where plants with extra biomass are removed completely
End_Time_Point_CGRemoval <- End_Time_Point %>% 
  filter(!biomass_removed>0)

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

#for models where plants with extra biomass are removed completely
NPP_Join_CGRemoval <- NPP_Join %>% 
  filter(!biomass_removed>0)

#join leaf data with plantID
Leaf_Data_Join <- Leaf_Data %>%
  #remove plants with no leaf data
  drop_na(wet_leaf_weight) %>% 
  full_join(PlantID_BiomassRemoved) %>%
  drop_na(leaf_number) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(LDMC = dry_leaf_weight / wet_leaf_weight) %>%
  mutate(SLA = leaf_area / dry_leaf_weight) %>% 
  select(overall_group,spring_plant_ID,leaf_number,wet_leaf_weight,dry_leaf_weight,leaf_area,leaf_thickness,SLA,LDMC,biomass_removed)

#for models where plants with extra biomass are removed completely
Leaf_Data_Join_CGRemoval <- Leaf_Data_Join %>% 
  filter(!biomass_removed>0)


#####################################################################
# Relative Growth Rate - Leaf Number #
Through_Time_Final2 <- Through_Time_Join %>%
  drop_na(leaf_num) 

# Week 1-2 slope #
leafnum_week1 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "1") %>%
  rename(leafnum_wk1 = leaf_num) %>%
  rename(week_1 = week_num)
leafnum_week2 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "2") %>%
  rename(leafnum_wk2 = leaf_num) %>%
  rename(week_2 = week_num)
leafnum_week1_2 <- leafnum_week1 %>%
  full_join(leafnum_week2) %>%
  mutate(slope = (leafnum_wk2 - leafnum_wk1)/1) #2 timepoints but the difference in x-axis (weeks) is 1

leafnum_W1_2 <- leafnum_week1_2 %>%
  mutate(overall_group = ifelse(overall_group=="Control-Control", "Control", 
              ifelse(overall_group=="Control-Heatwave", "Control",
              ifelse(overall_group=="Heatwave-Control", "Heatwave",
              ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W1-2") %>%
  select(overall_group, spring_plant_ID, slope, timepoint,biomass_removed)

  
# Week 3-5 slope #
leafnum_week3 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "3") %>%
  rename(leafnum_wk3 = leaf_num) %>%
  rename(week_3 = week_num)
leafnum_week5 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "5") %>%
  rename(leafnum_wk5 = leaf_num) %>%
  rename(week_5 = week_num)
leafnum_week3_5 <- leafnum_week3 %>%
  full_join(leafnum_week5) %>%
  mutate(slope = (leafnum_wk5 - leafnum_wk3)/2)#3 timepoints but the difference in x-axis (weeks) is 2


leafnum_W3_5 <- leafnum_week3_5 %>%
  mutate(overall_group = ifelse(overall_group=="Control-Control", "Control", 
                        ifelse(overall_group=="Control-Heatwave", "Control",
                        ifelse(overall_group=="Heatwave-Control", "Heatwave",
                       ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W3-5") %>%
  select(overall_group, spring_plant_ID, slope, timepoint,biomass_removed)


# Week 9-18 slope #
leafnum_week9 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "9") %>%
  rename(leafnum_wk9 = leaf_num) %>%
  rename(week_9 = week_num)
leafnum_week18 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "18") %>%
  rename(leafnum_wk18 = leaf_num) %>%
  rename(week_18 = week_num)
leafnum_W9_18 <- leafnum_week9 %>%
  full_join(leafnum_week18) %>%
  mutate(slope = (leafnum_wk18 - leafnum_wk9)/9) %>%  #2 timepoints but the difference in x-axis (weeks) is 9 %>%
  add_column(timepoint = "W9-18") %>%
  na.omit(slope) %>% 
  select(overall_group, spring_plant_ID, slope, timepoint,biomass_removed)
#dataframe with no crabgrass plants
leafnum_W9_18_NCG <- leafnum_W9_18 %>%
  filter(biomass_removed==0) 



# Week 19-22 slope #
leafnum_week19 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "19") %>%
  rename(leafnum_wk19 = leaf_num) %>%
  rename(week_19 = week_num)
leafnum_week22 <- Through_Time_Final2 %>%
  dplyr::select(-c(survival, max_leaf_length, max_plant_height, soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "22") %>%
  rename(leafnum_wk22 = leaf_num) %>%
  rename(week_22 = week_num)
leafnum_W19_22 <- leafnum_week19 %>%
  full_join(leafnum_week22) %>%
  mutate(slope = (leafnum_wk22 - leafnum_wk19)/3) %>%
  add_column(timepoint = "W19-22") %>%
  select(overall_group, spring_plant_ID, slope, timepoint,biomass_removed)
#dataframe with no crabgrass plants
leafnum_W19_22_NCG <- leafnum_W19_22 %>%
  filter(biomass_removed==0) 


# merge all data frames together #
finalLeafNumSlope <- leafnum_W1_2 %>% 
  rbind(leafnum_W3_5) %>%
  rbind(leafnum_W9_18) %>%
  rbind(leafnum_W19_22)
  


ggplot(finalLeafNumSlope,aes(x=factor(timepoint, level=c('W1-2', 'W3-5', 'W9-18', 'W19-22')),y=slope, fill=overall_group))+
  geom_boxplot()


#### LeafNum TP1 Stats ####
# check for normality #
test <- lm(data = leafnum_W1_2, slope ~ overall_group)
ols_plot_resid_hist(test)
ols_test_normality(test) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

# Run simplest model, anova comparing SLA to overall_group
LL_TP1_model <- aov(slope ~ overall_group, data = leafnum_W1_2)
summary(LL_TP1_model) #p=0.238
#which one? using anova for now since it's what we use for the rest of the data & wilcox test gives same results
#wilcox.test(leafnum_W1_2$slope~leafnum_W1_2$overall_group) #0.297


#### LeafNum TP2 Stats ####
# check for normality #
Normality_test_TP2 <- lm(data = leafnum_W3_5, slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP2)
ols_test_normality(Normality_test_TP2) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

# Run simplest model, anova comparing SLA to overall_group
LL_TP2_model <- aov(slope ~ overall_group, data = leafnum_W3_5)
summary(LL_TP2_model) #p=0.109

#### LeafNum TP3 Stats ####
# check for normality #
Normality_test_TP3 <- lm(data = leafnum_W9_18, slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP3)
ols_test_normality(Normality_test_TP3) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

#try transformations
leafnum_W9_18 <- leafnum_W9_18 %>%
  mutate(TF_slope = sign(slope) * log(1 + abs(slope)))

Normality_test_TP3_TF <- lm(data = leafnum_W9_18, TF_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP3_TF)
ols_test_normality(Normality_test_TP3_TF)

# Run simplest model, anova comparing SLA to overall_group
LL_TP3_model <- aov(TF_slope ~ overall_group, data = leafnum_W9_18)
summary(LL_TP3_model) #p=0.00105
#run post hoc test

#run model not using any plants that had biomass removed
LL_TP3_model_noCG <- aov(slope ~ overall_group, data = leafnum_W9_18_NCG)
summary(LL_TP3_model_noCG) #p=0.00134

# run model accounting for biomass removed
LL_TP3_model_biomass <- lmerTest::lmer(slope ~ overall_group + (1 | biomass_removed), data = leafnum_W9_18)
anova(LL_TP3_model_biomass) #p=0.001012


#### LeafNum TP4 Stats ####
# check for normality #
Normality_test_TP4 <- lm(data = leafnum_W19_22, slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP4)
ols_test_normality(Normality_test_TP4) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
LL_TP4_model <- aov(TF_slope ~ overall_group, data = leafnum_W19_22)
summary(LL_TP4_model) #p=4.13e-10
#run post hoc test

#run model not using any plants that had biomass removed
LL_TP4_model_noCG <- aov(slope ~ overall_group, data = leafnum_W19_22_NCG)
summary(LL_TP4_model_noCG) #p=1.76e-07

# run model accounting for biomass removed
LL_TP4_model_biomass <- lmerTest::lmer(slope ~ overall_group + (1 | biomass_removed), data = leafnum_W19_22)
anova(LL_TP4_model_biomass) #p=1.838e-10

