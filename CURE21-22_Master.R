#### Master Script for CURE 21-22 Manuscript ####


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(lmerTest)
library(stringr)  
library(multcomp)
library(tidyverse)
library(grid)
library(patchwork)

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

Through_Time_Join_NCG<-Through_Time_Join %>% 
  filter(biomass_removed==0)

#### Through Time Relative Growth Rate Data: one GR ####

# Slope #

leafnum_week1_slope <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "1") %>%
  rename(survival_wk1=survival) %>% 
  rename(leafnum_wk1 = leaf_num) %>%
  rename(maxLL_wk1 = max_leaf_length) %>% 
  rename(maxPH_wk1 = max_plant_height) %>% 
  rename(week_1 = week_num)


leafnum_week22_slope <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "22") %>%
  rename(survival_wk22=survival) %>% 
  rename(leafnum_wk22 = leaf_num) %>%
  rename(maxLL_wk22 = max_leaf_length) %>% 
  rename(maxPH_wk22 = max_plant_height) %>% 
  rename(week_22 = week_num)

leafnum_W1_22 <- leafnum_week1_slope %>%
  full_join(leafnum_week22_slope) %>%
  mutate(leafnum_slope = (leafnum_wk22 - leafnum_wk1)/21) %>%  
  mutate(maxLL_slope = (maxLL_wk22 - maxLL_wk1)/21) %>% 
  mutate(maxPH_slope = (maxPH_wk22 - maxPH_wk1)/21) %>% 
  add_column(timepoint = "W1-22") %>%
  select(overall_group, spring_plant_ID, timepoint, leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed) %>% 
  na.omit()

#### Leaf Number single GR Figure ####
Leafnum_GR_Graph<-ggplot(leafnum_W1_22,aes(x = overall_group,y = leafnum_slope, fill = overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Relative Growth Rate") +
  expand_limits(y=c(2,2))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Leaf Number single TP GR Stats ####
# check for normality #
Normality_test_LeafNum <- lm(data = leafnum_W1_22, leafnum_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_LeafNum )
ols_test_normality(Normality_test_LeafNum ) 

# Run simplest model, anova comparing SLA to overall_group
LeafNum_singleGR_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W1_22)
summary(LeafNum_singleGR_model) #p=0.0.000144
#post-hoc tests
summary(glht(LeafNum_singleGR_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH")) 


#### Max Leaf Length single GR Figure ####
MaxLL_GR_Graph<-ggplot(leafnum_W1_22,aes(x = overall_group,y = maxLL_slope, fill = overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Relative Growth Rate") +
  expand_limits(y=c(2,2))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### max leaf length single TP GR Stats ####
# check for normality #
Normality_test_MaxLL <- lm(data = leafnum_W1_22, maxLL_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_MaxLL)
ols_test_normality(Normality_test_MaxLL) 

# Run simplest model, anova comparing SLA to overall_group
MaxLL_singleGR_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W1_22)
summary(MaxLL_singleGR_model) #p=0.0.0953
#post hoc test

#### Max Plant Height single GR Figure ####
MaxPH_GR_Graph<-ggplot(leafnum_W1_22,aes(x = overall_group,y = maxPH_slope, fill = overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Relative Growth Rate") +
  expand_limits(y=c(-10,25))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Max Plant Height single GR Stats ####
# check for normality #
Normality_test_MaxPH <- lm(data = leafnum_W1_22, maxPH_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_MaxPH)
ols_test_normality(Normality_test_MaxPH) 

# Run simplest model, anova comparing SLA to overall_group
MaxPH_singleGR_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W1_22)
summary(MaxPH_singleGR_model) #p=0.205
#post hoc test

#### Through Time Relative Growth Rate Data: timepoints ####
  
# Week 1-2 slope #

leafnum_week1 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "1") %>%
  rename(survival_wk1=survival) %>% 
  rename(leafnum_wk1 = leaf_num) %>%
  rename(maxLL_wk1 = max_leaf_length) %>% 
  rename(maxPH_wk1 = max_plant_height) %>% 
  rename(week_1 = week_num)


leafnum_week2 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "2") %>%
  rename(survival_wk2=survival) %>% 
  rename(leafnum_wk2 = leaf_num) %>%
  rename(maxLL_wk2 = max_leaf_length) %>% 
  rename(maxPH_wk2 = max_plant_height) %>% 
  rename(week_2 = week_num)
  
leafnum_W1_2 <- leafnum_week1 %>%
  full_join(leafnum_week2) %>%
  mutate(leafnum_slope = (leafnum_wk2 - leafnum_wk1)/1) %>%  #2 timepoints but the difference in x-axis (weeks) is 1
  mutate(maxLL_slope = (maxLL_wk2 - maxLL_wk1)/1) %>% 
  mutate(maxPH_slope = (maxPH_wk2 - maxPH_wk1)/1) %>% 
  mutate(overall_group =ifelse(overall_group=="Control-Control", "Control",
                        ifelse(overall_group=="Control-Heatwave", "Control",
                        ifelse(overall_group=="Heatwave-Control", "Heatwave",
                        ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W1-2") %>%
  select(overall_group, spring_plant_ID, timepoint, leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed)


# Week 3-4 slope #
leafnum_week3 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "3") %>%
  rename(survival_wk3=survival) %>% 
  rename(leafnum_wk3 = leaf_num) %>% 
  rename(maxLL_wk3 = max_leaf_length) %>% 
  rename(maxPH_wk3 = max_plant_height) %>% 
  rename(week_3 = week_num)

leafnum_week4 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "4") %>%
  rename(survival_wk4=survival) %>% 
  rename(leafnum_wk4 = leaf_num) %>%
  rename(maxLL_wk4 = max_leaf_length) %>% 
  rename(maxPH_wk4 = max_plant_height) %>% 
  rename(week_4 = week_num)

leafnum_W3_4 <- leafnum_week3 %>%
  full_join(leafnum_week4) %>%
  mutate(leafnum_slope = (leafnum_wk4 - leafnum_wk3)/1) %>%  #3 timepoints but the difference in x-axis (weeks) is 2
  mutate(maxLL_slope = (maxLL_wk4 - maxLL_wk3)/1) %>% 
  mutate(maxPH_slope = (maxPH_wk4 - maxPH_wk3)/1) %>% 
  mutate(overall_group=ifelse(overall_group=="Control-Control", "Control", 
                        ifelse(overall_group=="Control-Heatwave", "Control",
                        ifelse(overall_group=="Heatwave-Control", "Heatwave",
                        ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W3-4") %>%
  select(overall_group, spring_plant_ID, timepoint, leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed) %>% 
  na.omit()

# Week 5-9 slope #

leafnum_week5 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "5") %>%
  rename(survival_wk5=survival) %>% 
  rename(leafnum_wk5 = leaf_num) %>%
  rename(maxLL_wk5 = max_leaf_length) %>% 
  rename(maxPH_wk5 = max_plant_height) %>% 
  rename(week_5 = week_num)

leafnum_week9 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "9") %>%
  rename(survival_wk9 = survival) %>% 
  rename(leafnum_wk9 = leaf_num) %>% 
  rename(maxLL_wk9 = max_leaf_length) %>% 
  rename(maxPH_wk9 = max_plant_height) %>% 
  rename(week_9 = week_num)

leafnum_W5_9 <- leafnum_week5 %>%
  full_join(leafnum_week9) %>%
  mutate(leafnum_slope = (leafnum_wk9 - leafnum_wk5)/4) %>%  #2 timepoints but the difference in x-axis (weeks) is 9 
  mutate(maxLL_slope = (maxLL_wk9 - maxLL_wk5)/4) %>% 
  mutate(maxPH_slope = (maxPH_wk9 - maxPH_wk5)/4) %>% 
  mutate(overall_group=ifelse(overall_group=="Control-Control", "Control",ifelse(overall_group=="Control-Heatwave", "Control",ifelse(overall_group=="Heatwave-Control", "Heatwave",ifelse(overall_group=="Heatwave-Heatwave", "Heatwave", overall_group))))) %>%
  add_column(timepoint = "W5-9") %>%
  select(overall_group, spring_plant_ID, timepoint,leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed) %>% 
  na.omit()


# Week 18-20 slope #

leafnum_week18 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "18") %>%
  rename(survival_wk18 = survival) %>% 
  rename(leafnum_wk18 = leaf_num) %>% 
  rename(maxLL_wk18 = max_leaf_length) %>% 
  rename(maxPH_wk18 = max_plant_height) %>% 
  rename(week_18 = week_num)

leafnum_week20 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "20") %>%
  rename(survival_wk20 = survival) %>% 
  rename(leafnum_wk20 = leaf_num) %>% 
  rename(maxLL_wk20 = max_leaf_length) %>% 
  rename(maxPH_wk20 = max_plant_height) %>% 
  rename(week_20 = week_num)

leafnum_W18_20 <- leafnum_week18 %>%
  full_join(leafnum_week20) %>%
  mutate(leafnum_slope = (leafnum_wk20 - leafnum_wk18)/2) %>%  #2 timepoints but the difference in x-axis (weeks) is 9 
  mutate(maxLL_slope = (maxLL_wk20 - maxLL_wk18)/2) %>% 
  mutate(maxPH_slope = (maxPH_wk20 - maxPH_wk18)/2) %>% 
  add_column(timepoint = "W18-20") %>%
  select(overall_group, spring_plant_ID, timepoint,leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed) %>% 
  na.omit()

# Week 21-22 slope #
leafnum_week21 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "21") %>%
  rename(survival_wk21 = survival) %>% 
  rename(leafnum_wk21 = leaf_num) %>% 
  rename(maxLL_wk21 = max_leaf_length) %>% 
  rename(maxPH_wk21 = max_plant_height) %>% 
  rename(week_21 = week_num)

leafnum_week22 <- Through_Time_Join_NCG %>%
  dplyr::select(-c(soil_moisture, light_avail, air_temp, humidity,plant_stress)) %>%
  filter(week_num == "22") %>%
  rename(survival_wk22 = survival) %>% 
  rename(leafnum_wk22 = leaf_num) %>% 
  rename(maxLL_wk22 = max_leaf_length) %>% 
  rename(maxPH_wk22 = max_plant_height) %>% 
  rename(week_22 = week_num)

leafnum_W21_22 <- leafnum_week21 %>%
  full_join(leafnum_week22) %>%
  mutate(leafnum_slope = (leafnum_wk22 - leafnum_wk21)/1) %>%
  mutate(maxLL_slope = (maxLL_wk22 - maxLL_wk21)/1) %>% 
  mutate(maxPH_slope = (maxPH_wk22 - maxPH_wk21)/1) %>% 
  add_column(timepoint = "W21-22") %>%
  select(overall_group, spring_plant_ID, timepoint, leafnum_slope, maxLL_slope,maxPH_slope,biomass_removed) %>% 
  na.omit()

### merge all data frames together #
finalLeafNumSlope <- leafnum_W1_2 %>% 
  rbind(leafnum_W3_4) %>%
  rbind(leafnum_W5_9) %>%
  rbind(leafnum_W18_20) %>%
  rbind(leafnum_W21_22)

#### Leaf Number Figure ####
Leafnum_TPs_Graph<-ggplot(finalLeafNumSlope,aes(x=factor(timepoint, level=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22')),y=leafnum_slope, fill=overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Timepoint",y ="Relative Growth Rate") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(-100,50))+
  #change color of treatments
  scale_fill_manual(values=c("#76AFE8", "#76AFE8","#E6E291","#CA7E77","#88A76E","#CA7E77")) +
  scale_x_discrete(breaks=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22'),
                   labels=c("Pre HW", "HW1", "R1","HW2", "R2"))
#save at 2000 x 1500

#### Leaf Number TP1 Stats ####
# check for normality #
Normality_test_TP1 <- lm(data = leafnum_W1_2, leafnum_slope ~ overall_group)
ols_plot_resid_hist(test)
ols_test_normality(test) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

# Run simplest model, anova comparing SLA to overall_group
LL_TP1_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W1_2)
summary(LL_TP1_model) #p=0.238
#which one? using anova for now since it's what we use for the rest of the data & wilcox test gives same results
#wilcox.test(leafnum_W1_2$slope~leafnum_W1_2$overall_group) #0.297


#### Leaf Number TP2 Stats ####
# check for normality #
Normality_test_TP2 <- lm(data = leafnum_W3_4, leafnum_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP2)
ols_test_normality(Normality_test_TP2) 

# Run simplest model, anova comparing SLA to overall_group
LL_TP2_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W3_4)
summary(LL_TP2_model) #p=0.124

#### Leaf Number TP3 Stats ####
# check for normality #
Normality_test_TP3 <- lm(data = leafnum_W5_9, leafnum_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP3)
ols_test_normality(Normality_test_TP3)

# Run simplest model, an ova comparing SLA to overall_group
LL_TP3_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W5_9)
summary(LL_TP3_model) #p=0.911

#### Leaf Number TP4 Stats ####
# check for normality #
Normality_test_TP4 <- lm(data = leafnum_W18_20, leafnum_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP4)
ols_test_normality(Normality_test_TP4) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
LL_TP4_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W18_20)
summary(LL_TP4_model) #p=4.73e-05e-10
#post-hoc tests
summary(glht(LL_TP4_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH")) 

#run post hoc test

#### Leaf Number TP5 Stats ####
# check for normality #
Normality_test_TP5 <- lm(data = leafnum_W21_22, leafnum_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP5)
ols_test_normality(Normality_test_TP5) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
LL_TP5_model <- aov(leafnum_slope ~ overall_group, data = leafnum_W21_22)
summary(LL_TP5_model) #p=6.03e-06
#run post hoc test
summary(glht(LL_TP5_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH")) 

#### max leaf length Figure ####
MaxLL_TPs_Graph<-ggplot(finalLeafNumSlope,aes(x=factor(timepoint, level=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22')),y=maxLL_slope, fill=overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Timepoint",y ="Relative Growth Rate") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(-200,200))+
  #change color of treatments
  scale_fill_manual(values=c("#76AFE8", "#76AFE8","#E6E291","#CA7E77","#88A76E","#CA7E77")) +
  scale_x_discrete(breaks=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22'),
                   labels=c("Pre HW", "HW1", "R1","HW2", "R2"))
#save at 2000 x 1500

#### Max Leaf Length TP1 Stats ####
# check for normality #
Normality_test_TP1 <- lm(data = leafnum_W1_2, maxLL_slope ~ overall_group)
ols_plot_resid_hist(test)
ols_test_normality(test) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

# Run simplest model, anova comparing SLA to overall_group
maxLL_TP1_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W1_2)
summary(maxLL_TP1_model) #p=0.0443
#which one? using anova for now since it's what we use for the rest of the data & wilcox test gives same results
#wilcox.test(leafnum_W1_2$slope~leafnum_W1_2$overall_group) #0.297


#### Max Leaf Length TP2 Stats ####
# check for normality #
Normality_test_TP2 <- lm(data = leafnum_W3_4, maxLL_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP2)
ols_test_normality(Normality_test_TP2) 

# Run simplest model, anova comparing SLA to overall_group
maxLL_TP2_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W3_4)
summary(maxLL_TP2_model) #p=0.858

#### Max Leaf Length TP3 Stats ####
# check for normality #
Normality_test_TP3 <- lm(data = leafnum_W5_9, maxLL_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP3)
ols_test_normality(Normality_test_TP3)

# Run simplest model, an ova comparing SLA to overall_group
maxLL_TP3_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W5_9)
summary(maxLL_TP3_model) #p=0.0637

#### Max Leaf Length TP4 Stats ####
# check for normality #
Normality_test_TP4 <- lm(data = leafnum_W18_20, maxLL_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP4)
ols_test_normality(Normality_test_TP4) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
maxLL_TP4_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W18_20)
summary(maxLL_TP4_model) #p=0.0107
#run post hoc test

#### Max Leaf Length TP5 Stats ####
# check for normality #
Normality_test_TP5 <- lm(data = leafnum_W21_22, maxLL_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP5)
ols_test_normality(Normality_test_TP5) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
maxLL_TP5_model <- aov(maxLL_slope ~ overall_group, data = leafnum_W21_22)
summary(maxLL_TP5_model) #p=0.691
#run post hoc test

#### max plant height Figure ####
MaxPH_TPs_Graph<-ggplot(finalLeafNumSlope,aes(x=factor(timepoint, level=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22')),y=maxPH_slope, fill=overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Timepoint",y ="Relative Growth Rate") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(-200,200))+
  #change color of treatments
  scale_fill_manual(values=c("#76AFE8", "#76AFE8","#E6E291","#CA7E77","#88A76E","#CA7E77")) +
  scale_x_discrete(breaks=c('W1-2', 'W3-4', 'W5-9', 'W18-20','W21-22'),
                   labels=c("Pre HW", "HW1", "R1","HW2", "R2"))
#save at 2000 x 1500

#### Max Plant Height TP1 Stats ####
# check for normality #
Normality_test_TP1 <- lm(data = leafnum_W1_2, maxPH_slope ~ overall_group)
ols_plot_resid_hist(test)
ols_test_normality(test) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option with one test being above 0.05

# Run simplest model, anova comparing SLA to overall_group
maxPH_TP1_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W1_2)
summary(maxPH_TP1_model) #p=0.676
#which one? using anova for now since it's what we use for the rest of the data & wilcox test gives same results
#wilcox.test(leafnum_W1_2$slope~leafnum_W1_2$overall_group) #0.297


#### Max Plant Height TP2 Stats ####
# check for normality #
Normality_test_TP2 <- lm(data = leafnum_W3_4, maxPH_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP2)
ols_test_normality(Normality_test_TP2) 

# Run simplest model, anova comparing SLA to overall_group
maxPH_TP2_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W3_4)
summary(maxPH_TP2_model) #p=0.268

#### Max Plant Height TP3 Stats ####
# check for normality #
Normality_test_TP3 <- lm(data = leafnum_W5_9, maxPH_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP3)
ols_test_normality(Normality_test_TP3)

# Run simplest model, an ova comparing SLA to overall_group
maxPH_TP3_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W5_9)
summary(maxPH_TP3_model) #p=0.956

#### Max Plant Height TP4 Stats ####
# check for normality #
Normality_test_TP4 <- lm(data = leafnum_W18_20, maxPH_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP4)
ols_test_normality(Normality_test_TP4) # want all 4 p-values in output to be >0.05 for normality, we tried to transform the data but non-transformed data was the best option

# Run simplest model, anova comparing SLA to overall_group
maxPH_TP4_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W18_20)
summary(maxPH_TP4_model) #p=0.137
#run post hoc test

#### Max Plant Height TP5 Stats ####
# check for normality #
Normality_test_TP5 <- lm(data = leafnum_W21_22, maxPH_slope ~ overall_group)
ols_plot_resid_hist(Normality_test_TP5)
ols_test_normality(Normality_test_TP5) 

# Run simplest model, anova comparing SLA to overall_group
maxPH_TP5_model <- aov(maxPH_slope ~ overall_group, data = leafnum_W21_22)
summary(maxPH_TP5_model) #p=0.054
#run post hoc test

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

#### Wk22 Max Leaf Length Graph ####

MaxLL_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = max_leaf_length, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Avg Leaf Length (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=600)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Wk22 Max Leaf Length Stats ####

# Run simplest model, anova comparing SLA to overall_group
MaxLL_model <- aov(max_leaf_length ~ overall_group, data = End_Time_Point)
summary(MaxLL_model) #0.0766

#run model not using any plants that had biomass removed
MaxLL_model_noCG <- aov(max_leaf_length ~ overall_group, data = End_Time_Point_CGRemoval)
summary(MaxLL_model_noCG) #p=0.0393
#post-hoc tests
summary(glht(MaxLL_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH")) 

# run model accounting for biomass removed
MaxLL_model_biomass <- lmerTest::lmer(max_leaf_length ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(MaxLL_model_biomass) #p=0.07772

#### Wk22 Max Plant Height Graph ####

MaxPH_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = max_plant_height, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Avg Plant Height (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=400)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Wk22 Max Plant Height Stats ####

# Run simplest model, anova comparing SLA to overall_group
MaxPH_model <- aov(max_plant_height ~ overall_group, data = End_Time_Point)
summary(MaxPH_model) #0.00117

#run model not using any plants that had biomass removed
MaxPH_model_noCG <- aov(max_plant_height ~ overall_group, data = End_Time_Point_CGRemoval)
summary(MaxPH_model_noCG) #p=0.0131

# run model accounting for biomass removed
MaxPH_model_biomass <- lmerTest::lmer(max_plant_height ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(MaxPH_model_biomass) #p=0.001194

#### Wk22 Leaf Number Graph ####

Leaf_Num_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = leaf_num, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Avg Leaf Number") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=80)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500


#### Wk22 Leaf Number Stats ####

# Run simplest model, anova comparing SLA to overall_group
leaf_num_model <- aov(leaf_num ~ overall_group, data = End_Time_Point)
summary(leaf_num_model) #2.0e-09

#run model not using any plants that had biomass removed
leaf_num_model_noCG <- aov(leaf_num ~ overall_group, data = End_Time_Point_CGRemoval)
summary(leaf_num_model_noCG) #p=2.48e-09

# run model accounting for biomass removed
leaf_num_model_biomass <- lmerTest::lmer(leaf_num ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(leaf_num_model_biomass) #p=2.112e-09

#### Alive ANPP Graph ####

Alive_ANPP_Graph <- ggplot(NPP_Join_CGRemoval, aes(x = overall_group, y = alive_ANPP_g, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Alive ANPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=2)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Alive ANPP Stats ####

# Run simplest model, anova comparing SLA to overall_group
alive_ANPP_model <- aov(alive_ANPP_g ~ overall_group, data = NPP_Join)
summary(alive_ANPP_model) #0.000334

#run model not using any plants that had biomass removed
alive_ANPP_model_noCG <- aov(alive_ANPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(alive_ANPP_model_noCG) #0.00767

# run model accounting for biomass removed
alive_ANPP_model_biomass <- lmerTest::lmer(alive_ANPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(alive_ANPP_model_biomass) #0.0003199

### Dead ANPP Graph ####

Dead_ANPP_Graph <- ggplot(NPP_Join_CGRemoval, aes(x = overall_group, y = dead_ANPP_g, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Dead ANPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=2)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500


#### Dead ANPP Stats ####

# Run simplest model, anova comparing SLA to overall_group
dead_ANPP_model <- aov(dead_ANPP_g ~ overall_group, data = NPP_Join)
summary(dead_ANPP_model) #0.0492

#run model not using any plants that had biomass removed
dead_ANPP_model_noCG <- aov(dead_ANPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(dead_ANPP_model_noCG) #0.0271

# run model accounting for biomass removed
dead_ANPP_model_biomass <- lmerTest::lmer(dead_ANPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(dead_ANPP_model_biomass) #0.01366

### Total ANPP Graph ####

ANPP_Graph <- ggplot(NPP_Join_CGRemoval, aes(x = overall_group, y = total_ANPP_g, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Total ANPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=3)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Total ANPP Stats ####

# Run simplest model, anova comparing SLA to overall_group
total_ANPP_model <- aov(total_ANPP_g ~ overall_group, data = NPP_Join)
summary(total_ANPP_model) #0.0929

#run model not using any plants that had biomass removed
total_ANPP_model_noCG <- aov(total_ANPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(total_ANPP_model_noCG) #0.051

# run model accounting for biomass removed
total_ANPP_model_biomass <- lmerTest::lmer(total_ANPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(total_ANPP_model_biomass) #0.05528

### BNPP Graph ####

BNPP_Graph <- ggplot(NPP_Join_CGRemoval, aes(x = overall_group, y = BNPP_g, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="BNPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=2)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500


#### BNPP Stats ####

# Run simplest model, anova comparing SLA to overall_group
BNPP_model <- aov(BNPP_g ~ overall_group, data = NPP_Join)
summary(BNPP_model) #0.00897

#run model not using any plants that had biomass removed
BNPP_model_noCG <- aov(BNPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(BNPP_model_noCG) #0.0119

# run model accounting for biomass removed
BNPP_model_biomass <- lmerTest::lmer(BNPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(BNPP_model_biomass) #0.007883

### NPP Graph ####

NPP_Graph <- ggplot(NPP_Join_CGRemoval, aes(x = overall_group, y = NPP, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Total NPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,4))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500


#### NPP Stats ####

# Run simplest model, anova comparing SLA to overall_group
NPP_model <- aov(NPP ~ overall_group, data = NPP_Join)
summary(NPP_model) #0.0172

#run model not using any plants that had biomass removed
NPP_model_noCG <- aov(NPP ~ overall_group, data = NPP_Join_CGRemoval)
summary(NPP_model_noCG) #0.0101

# run model accounting for biomass removed
NPP_model_biomass <- lmerTest::lmer(NPP ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(NPP_model_biomass) #0.009993


#### SLA Graph ####

SLA_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = SLA, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y =expression ("Specific Leaf Area"~(mm^2/g))) +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=800)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### SLA Stats ####

# Run simplest model, anova comparing SLA to overall_group
SLA_model <- aov(SLA ~ overall_group, data = Leaf_Data_Join)
summary(SLA_model) #1.1e-05

#run model not using any plants that had biomass removed
SLA_model_noCG <- aov(SLA ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(SLA_model_noCG) #p=0.00234

# run model accounting for biomass removed
SLA_model_biomass <- lmerTest::lmer(SLA ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(SLA_model_biomass) #p=8.0788e-06

#### Leaf Thickness Graph ####

LeafThickness_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = leaf_thickness, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Thickness (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=0.4)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### Leaf Thickness Stats ####

# Run simplest model, anova comparing SLA to overall_group
Thickness_model <- aov(leaf_thickness ~ overall_group, data = Leaf_Data_Join)
summary(Thickness_model) #2.72e-13

#run model not using any plants that had biomass removed
Thickness_model_noCG <- aov(leaf_thickness ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(Thickness_model_noCG) #p=1.32e-05

# run model accounting for biomass removed
Thickness_model_biomass <- lmerTest::lmer(leaf_thickness ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(Thickness_model_biomass) #p=2.588e-11

#### LDMC Graph ####

LDMC_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = LDMC, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Dry Matter Content") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=1.5)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#save at 2000 x 1500

#### LDMC Stats ####

# Run simplest model, anova comparing SLA to overall_group
LDMC_model <- aov(LDMC ~ overall_group, data = Leaf_Data_Join)
summary(LDMC_model) #3.68e-07

#run model not using any plants that had biomass removed
LDMC_model_noCG <- aov(LDMC ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(LDMC_model_noCG) #p=1.03e-07

# run model accounting for biomass removed
LDMC_model_biomass <- lmerTest::lmer(LDMC ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(LDMC_model_biomass) #p=1.433e-08

#### Paper Figures ####


#### Figure 1: Abiotics ####

#### Figure 2: End Time Point ####

## Max Leaf Length single GR Figure ##

leafnum_W1_22$overall_group<-gsub("-"," ", leafnum_W1_22$overall_group)

MaxLL_GR_Graph<-ggplot(leafnum_W1_22,aes(x = overall_group,y = maxLL_slope, fill = overall_group))+
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Relative Growth Rate (mm/week)") +
  expand_limits(y=c(30,-10))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())

## Wk22 Max Plant Height Graph ##

End_Time_Point_CGRemoval$overall_group<-gsub("-"," ", End_Time_Point_CGRemoval$overall_group)

MaxPH_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = max_plant_height, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Average Plant Height (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,400))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())

## Wk22 Max Leaf Length Graph ##
MaxLL_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = max_leaf_length, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Average Leaf Length (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,600))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

## Wk22 Leaf Number Graph ##
Leaf_Num_Graph <- ggplot(End_Time_Point_CGRemoval, aes(x = overall_group, y = leaf_num, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Average Leaf Number") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,80))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  

#Create Figure
MaxLL_GR_Graph+
  MaxPH_Graph+
  MaxLL_Graph+
  Leaf_Num_Graph+
  plot_layout(ncol = 2,nrow = 2)
#save at 3500 x 3000


#### Figure 3: NPP ####

#### Figure 4: Traits ####

Leaf_Data_Join_CGRemoval$overall_group<-gsub("-"," ", Leaf_Data_Join_CGRemoval$overall_group)

## SLA Graph ##

SLA_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = SLA, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y =expression ("Specific Leaf Area"~(mm^2/g))) +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=800)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())

## LDMC Graph ##

LDMC_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = LDMC, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Dry Matter Content (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=1.5)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())


## Leaf Thickness Graph ##

LeafThickness_Graph <- ggplot(Leaf_Data_Join_CGRemoval, aes(x = overall_group, y = leaf_thickness, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Thickness (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=0.4)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#E6E291","#88A76E","#CA7E77")) +
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


#Create Figure
SLA_Graph+
  LDMC_Graph+
  LeafThickness_Graph+
  plot_layout(ncol = 1,nrow = 3)
#save at 3500 x 3000