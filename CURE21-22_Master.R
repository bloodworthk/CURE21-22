#### Master Script for CURE 21-22 Manuscript ####


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(lmerTest)
library(lme4)
library(stringr)  
library(multcomp)
library(tidyverse)
library(grid)
library(car)
library(olsrr)
library(patchwork)

#### Set working directory ####
#Bloodworth: mac
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

#read in biomass removed data
Biomass_Removed <- read.csv("removed_biomass.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "factor")) %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  select(fall_treatment, fall_day, fall_plant, overall_group, biomass_removed)

#read in plant ID meta-data
PlantID <- read.csv("plant_IDs.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "factor", "character", "factor", "factor", "factor", "factor")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  separate(fall_plant_ID,c("fall_day","fall_treatment","fall_plant"), sep = "_") %>% 
  #remove fall P185, P190, P193 because they only contained crabgrass throughout experiment
  filter(fall_plant!="P185" & fall_plant!="P190" & fall_plant!="P193")


#read in weekly data
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", colClasses = c("character", "character", "character", "character","character", "numeric", "factor", "factor", "factor", "factor", "factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-c(student_name,start_time,end_time))

#read in end timepoint ANPP & BNPP Measurements 
ANPP_BNPP <- read.csv("spring2022_ANPP_BNPP.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "character")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID) %>% 
  full_join(Biomass_Removed) %>% 
  filter(is.na(biomass_removed)) %>% 
  select(-c(notes,biomass_removed))

#read in end timepoint leaf metrics
Leaf_Data <- read.csv("spring2022_llp315cure_Leafcombo.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric")) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID) %>% 
  full_join(Biomass_Removed) %>% 
  filter(is.na(biomass_removed)) %>% 
  select(-c(notes,biomass_removed))


#### Clean Up Biomass Removed Data and add to Plant_ID####
Biomass_Removed$fall_treatment <- gsub(" ","",Biomass_Removed$fall_treatment)
Biomass_Removed$fall_day <- gsub(" ","",Biomass_Removed$fall_day)
Biomass_Removed$fall_plant <- gsub("p","P",Biomass_Removed$fall_plant)
Biomass_Removed$fall_plant <- gsub(" ","",Biomass_Removed$fall_plant)

PlantID_BiomassRemoved<-PlantID %>% 
  full_join(Biomass_Removed)
#replace NAs with zero for biomass_removed
PlantID_BiomassRemoved$biomass_removed[is.na(PlantID_BiomassRemoved$biomass_removed)] <- 0

PlantID_BiomassRemoved<-PlantID_BiomassRemoved %>% 
  filter(biomass_removed==0)

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
  drop_na(biomass_removed) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  select(overall_group, week_num,spring_plant_ID,survival,max_leaf_length,max_plant_height,leaf_num,plant_stress,soil_moisture,light_avail,air_temp,humidity)

Through_Time_Spring<-Through_Time %>% 
  filter(spring_plant_ID!="NA") %>% 
  select(-fall_plant_ID) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  drop_na(biomass_removed) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  select(overall_group, week_num,spring_plant_ID,survival,max_leaf_length,max_plant_height,leaf_num,plant_stress,soil_moisture,light_avail,air_temp,humidity)

#join fall and spring through time
Through_Time_Join<-Through_Time_Fall %>% 
  rbind(Through_Time_Spring) 

#### Clean Up EndPoint Data ####
# Create Week 22 Data
End_Time_Point_A_D <- Through_Time_Join %>% 
  filter(week_num=="22") %>% 
  select(-c(week_num,soil_moisture,light_avail,air_temp,humidity)) %>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group))))) 
End_Time_Point_A_D$treatment<-as.factor(End_Time_Point_A_D$treatment)

#make dataframe with only living plants
End_Time_Point <- End_Time_Point_A_D %>%  
  filter(survival=="A")
End_Time_Point$treatment<-as.factor(End_Time_Point$treatment)

#Create dataframe with alive and dead information
Alive_Dead_Status<- Through_Time_Join %>% 
  filter(week_num=="22") %>% 
  select(-c(week_num,soil_moisture,light_avail,air_temp,humidity)) %>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group)))))  %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  drop_na(biomass_removed) 
Alive_Dead_Status$treatment<-as.factor(Alive_Dead_Status$treatment)

#join ANPP_BNPP dataframe with plant ID and alive/dead status
NPP_Join_WDead <- Alive_Dead_Status %>%
  #join Plant data
  left_join(ANPP_BNPP) %>%
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(Alive_ANPP_WDead=ifelse(survival=="D",0,alive_ANPP_g))
NPP_Join_WDead$treatment<-as.factor(NPP_Join_WDead$treatment)

#join ANPP_BNPP dataframe with plant ID and alive/dead status
NPP_Join <- Alive_Dead_Status %>%
  #join Plant data
  left_join(ANPP_BNPP) %>%  
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  #remove any plants with NAs for Alive ANPP because they didn't get measured
  drop_na(alive_ANPP_g) %>% 
  #compute mutate step row by row
  rowwise() %>% 
  #total up the total NPP
  mutate(NPP = sum(c(total_ANPP_g, BNPP_g,rm.na=TRUE))) %>% 
  mutate(AliveNPP=sum(c(alive_ANPP_g,BNPP_g,rm.na=TRUE))) %>% 
  select(overall_group,spring_plant_ID,alive_ANPP_g,dead_ANPP_g,total_ANPP_g,BNPP_g,NPP,AliveNPP,survival, comments) %>% 
  mutate(ANPP_BNPP_ratio = alive_ANPP_g / BNPP_g) %>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group)))))  
NPP_Join$treatment<-as.factor(NPP_Join$treatment)

#make dataframe with only live plants
NPP_Join_Alive <- NPP_Join %>% 
  #remove all dead plants from biomass
  filter(survival!="D") %>%
  #remove plants where alive ANPP is 0
  filter(alive_ANPP_g!=0) %>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group))))) 
NPP_Join_Alive$treatment<-as.factor(NPP_Join_Alive$treatment)

#### Clean Leaf Trait Data ####

#join leaf data with plantID
Leaf_Data_Join <- Leaf_Data %>%
  #remove plants with no leaf data
  drop_na(wet_leaf_weight) %>% 
  full_join(PlantID_BiomassRemoved) %>%
  drop_na(leaf_number) %>% 
  mutate(spring_plant_ID=paste(spring_day,spring_treatment,spring_plant,sep="_")) %>% 
  mutate(spring_plant_ID_leaf=paste(spring_day,spring_treatment,spring_plant,leaf_number,sep="_")) %>% 
  #remove two leaves where dry leaf weight was higher than wet leaf weight
  filter(spring_plant_ID_leaf!="Tues_SH_115_1" & spring_plant_ID_leaf!="Thurs_SC_193_1") %>% 
  mutate(LDMC = dry_leaf_weight / wet_leaf_weight,na.rm=TRUE) %>%
  mutate(SLA = leaf_area / dry_leaf_weight,na.rm=TRUE) %>% 
  select(overall_group,spring_plant_ID,leaf_number,wet_leaf_weight,dry_leaf_weight,leaf_area,leaf_thickness,SLA,LDMC)%>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group))))) 
Leaf_Data_Join$treatment<-as.factor(Leaf_Data_Join$treatment)

#### Growth Rate Calculations ####
#Create Growth Rate by calculating slope of each week across plant individuals
#control-control
Control_Control_Slopes<-lmList(max_leaf_length~week_num|spring_plant_ID,data=subset(Through_Time_Join,overall_group=="Control-Control"))
#convert to dataframe
Control_Control_Slopes_DF<-coef(Control_Control_Slopes)
Control_Control_Slopes_DF2 <- tibble::rownames_to_column(Control_Control_Slopes_DF, "spring_plant_ID") %>% 
  rename(slope=week_num) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  filter(overall_group=="Control-Control")

#heatwave-control
heatwave_control_Slopes<-lmList(max_leaf_length~week_num|spring_plant_ID,data=subset(Through_Time_Join,overall_group=="Heatwave-Control"))
#convert to dataframe
heatwave_control_Slopes_DF<-coef(heatwave_control_Slopes)
heatwave_control_Slopes_DF2 <- tibble::rownames_to_column(heatwave_control_Slopes_DF, "spring_plant_ID") %>% 
  rename(slope=week_num) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  filter(overall_group=="Heatwave-Control")

#control-heatwave
control_heatwave_Slopes<-lmList(max_leaf_length~week_num|spring_plant_ID,data=subset(Through_Time_Join,overall_group=="Control-Heatwave"))
#convert to dataframe
control_heatwave_Slopes_DF<-coef(control_heatwave_Slopes)
control_heatwave_Slopes_DF2 <- tibble::rownames_to_column(control_heatwave_Slopes_DF, "spring_plant_ID") %>% 
  rename(slope=week_num) %>% 
separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  filter(overall_group=="Control-Heatwave")

#heatwave-heatwave
heatwave_heatwave_Slopes<-lmList(max_leaf_length~week_num|spring_plant_ID,data=subset(Through_Time_Join,overall_group=="Heatwave-Heatwave"))
#convert to dataframe
heatwave_heatwave_Slopes_DF<-coef(heatwave_heatwave_Slopes)
heatwave_heatwave_Slopes_DF2 <- tibble::rownames_to_column(heatwave_heatwave_Slopes_DF, "spring_plant_ID") %>% 
  rename(slope=week_num) %>% 
  separate(spring_plant_ID,c("spring_day","spring_treatment","spring_plant"), sep = "_") %>% 
  full_join(PlantID_BiomassRemoved) %>% 
  filter(overall_group=="Heatwave-Heatwave")
  
#Merge all growth rate slope dataframes
MaxLL_Slopes<-Control_Control_Slopes_DF2 %>% 
  rbind(heatwave_control_Slopes_DF2) %>% 
  rbind(control_heatwave_Slopes_DF2) %>% 
  rbind(heatwave_heatwave_Slopes_DF2) %>% 
  filter(slope>0) %>% 
  mutate(treatment=ifelse(overall_group=="Control-Control","Control",ifelse(overall_group=="Heatwave-Control","Early-HW",ifelse(overall_group=="Control-Heatwave","Late-HW",ifelse(overall_group=="Heatwave-Heatwave","Two-HWs",overall_group))))) 
MaxLL_Slopes$treatment<-as.factor(MaxLL_Slopes)

#### End Time Point Stats ####

#### Max Leaf Length Stats ####

# check for normality #
#non transformed data
Normality_test_MLL <- lm(data = End_Time_Point, max_leaf_length  ~ treatment)
ols_plot_resid_hist(Normality_test_MLL)
ols_test_normality(Normality_test_MLL)
#transform data
End_Time_Point<-End_Time_Point %>% 
  mutate(max_leaf_length_TF=sqrt(max_leaf_length))
#check normality of transformed data
Normality_test_MLL_TF <- lm(data = End_Time_Point, max_leaf_length_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_MLL_TF) 
ols_test_normality(Normality_test_MLL_TF) #best transformed with squareroot 

#check for homoscedascity
leveneTest(max_leaf_length_TF ~ treatment, data = End_Time_Point) #p = 0.2429 so > 0.05 so equal variance is met

#run model 
MaxLL_model <- aov(max_leaf_length_TF ~ treatment, data = End_Time_Point)
summary(MaxLL_model) #p=0.0337
summary(glht(MaxLL_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### Leaf Number Stats ####

# check for normality #
#non transformed data
Normality_test_LeafNum <- lm(data = End_Time_Point, leaf_num  ~ treatment)
ols_plot_resid_hist(Normality_test_LeafNum) 
ols_test_normality(Normality_test_LeafNum)
#transform data
End_Time_Point<-End_Time_Point %>% 
  mutate(leaf_num_TF=sqrt(leaf_num))
#check normality of transformed data
Normality_test_LeafNum_TF <- lm(data = End_Time_Point, leaf_num_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_LeafNum_TF) 
ols_test_normality(Normality_test_LeafNum_TF) #best transformed with sqrt 

#check for homoscedascity
leveneTest(leaf_num_TF ~ treatment, data = End_Time_Point) #p = 0.9065 so > 0.05 so equal variance is met

#run model 
leaf_num_model <- aov(leaf_num ~ treatment, data = End_Time_Point)
summary(leaf_num_model) #p=2.48e-09 
summary(glht(leaf_num_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### Max Leaf Length Lingle TP GGrowth Rate Stats #####

# check for normality #
#non transformed data
Normality_test_MaxLL <- lm(data = MaxLL_Slopes, slope ~ treatment)
ols_plot_resid_hist(Normality_test_MaxLL)
ols_test_normality(Normality_test_MaxLL)
#transform data
MaxLL_Slopes <-MaxLL_Slopes %>% 
  mutate(slope_TF=sqrt(slope))
# check for normality of transformed data#
Normality_test_MaxLL_TF <- lm(data = MaxLL_Slopes, slope_TF ~ treatment)
ols_plot_resid_hist(Normality_test_MaxLL_TF)
ols_test_normality(Normality_test_MaxLL_TF) #data transformed 

#check for homoscedascity
leveneTest(slope_TF ~ treatment, data = MaxLL_Slopes) #p = 0.923 so > 0.05 so equal variance is met

# Run anova comparing slopes to overall_group
MaxLL_GR_model <- aov(slope_TF ~ treatment, data = MaxLL_Slopes)
summary(MaxLL_GR_model) #p=0.281

#### NPP Stats ####

#### Alive ANPP Stats ####

# check for normality #
#non transformed data
Normality_test_Alive_ANPP <- lm(data = NPP_Join_Alive, alive_ANPP_g  ~ treatment)
ols_plot_resid_hist(Normality_test_Alive_ANPP) 
ols_test_normality(Normality_test_Alive_ANPP)
#transform data
NPP_Join_Alive<-NPP_Join_Alive %>% 
  mutate(alive_ANPP_g_TF=sqrt(alive_ANPP_g))
#check normality of transformed data
Normality_test_Alive_ANPP_TF <- lm(data = NPP_Join_Alive, alive_ANPP_g_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_Alive_ANPP_TF) 
ols_test_normality(Normality_test_Alive_ANPP_TF)#best transformed with sqrt 

#check for homoscedascity
leveneTest(alive_ANPP_g_TF ~ treatment, data = NPP_Join_Alive) #p = 0.597 so > 0.05 so equal variance is met

#run model 
Alive_ANPP_model <- aov(alive_ANPP_g_TF ~ treatment, data = NPP_Join_Alive)
summary(Alive_ANPP_model) #p=0.0691

#### BNPP Stats ####

# check for normality #
#non transformed data
Normality_test_BNPP <- lm(data = NPP_Join_Alive, BNPP_g  ~ treatment)
ols_plot_resid_hist(Normality_test_BNPP) 
ols_test_normality(Normality_test_BNPP) 
#transform data
NPP_Join_Alive<-NPP_Join_Alive %>% 
  mutate(BNPP_g_TF=log10(BNPP_g))
#check normality of transformed data
Normality_test_BNPP_TF <- lm(data = NPP_Join_Alive, BNPP_g_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_BNPP_TF) 
ols_test_normality(Normality_test_BNPP_TF)#best transformed with log

#check for homoscedascity
leveneTest(BNPP_g_TF ~ treatment, data = NPP_Join_Alive) #p = 0.162 so > 0.05 so equal variance is met

#run model 
BNPP_model <- aov(BNPP_g_TF ~ treatment, data = NPP_Join_Alive)
summary(BNPP_model) #p=0.0000827
summary(glht(BNPP_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### ANPP:BNPP Ratio Stats ####

# check for normality #
#non transformed data
Normality_test_Ratio <- lm(data = NPP_Join_Alive, ANPP_BNPP_ratio  ~ treatment)
ols_plot_resid_hist(Normality_test_Ratio) 
ols_test_normality(Normality_test_Ratio)
#transform data
NPP_Join_Alive<-NPP_Join_Alive %>% 
  mutate(ANPP_BNPP_ratio_TF=sqrt(ANPP_BNPP_ratio))
#check normality of transformed data
Normality_test_Ratio_TF <- lm(data = NPP_Join_Alive, ANPP_BNPP_ratio_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_Ratio_TF) 
ols_test_normality(Normality_test_Ratio_TF)#best transformed with sqrt 

#check for homoscedascity
leveneTest(ANPP_BNPP_ratio_TF ~ treatment, data = NPP_Join_Alive) #p = 0.3038 so > 0.05 so equal variance is met

#run model 
ANPP_BNPP_ratio_model <- aov(ANPP_BNPP_ratio_TF ~ treatment, data = NPP_Join_Alive)
summary(ANPP_BNPP_ratio_model) #p=0.38

#### Figure 2: End Time Point Figure ####

#### Figure 2A. End Timepoint Max Leaf Length Graph ####
End_Time_Point$treatment<-gsub("-"," ", End_Time_Point$treatment)
MaxLL_Graph <- ggplot(End_Time_Point, aes(x = treatment, y = max_leaf_length, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Maximum Leaf Length (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,800))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  annotate("text", x=0.6, y=800, label = "A.", size=20)+
  annotate("text", x=1, y=700, label = "a", size=20)+
  annotate("text", x=2, y=700, label = "b", size=20)+
  annotate("text", x=3, y=700, label = "ab", size=20)+
  annotate("text", x=4, y=700, label = "ab", size=20)

#### Figure 2B. End Timepoint Leaf Number Graph ####
Leaf_Num_Graph <- ggplot(End_Time_Point, aes(x = treatment, y = leaf_num, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Number") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,100))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  annotate("text", x=0.6, y=100, label = "B.", size=20)+
  annotate("text", x=1, y=85, label = "a", size=20)+
  annotate("text", x=2, y=85, label = "b", size=20)+
  annotate("text", x=3, y=85, label = "c", size=20)+
  annotate("text", x=4, y=85, label = "b", size=20)

#### Figure 2C. End Time Point Max Leaf Length Growth Rate Figure ####
MaxLL_Slopes$treatment<-gsub("-"," ", MaxLL_Slopes$treatment)
MaxLL_GR_Graph<-ggplot(MaxLL_Slopes,aes(x = treatment,y = slope, fill = treatment))+
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Relative Growth Rate (mm/week)") +
  expand_limits(y=c(0,25))+
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  annotate("text", x=0.6, y=25, label = "C.", size=20)

#### Figure 2D: Total Alive ANPP Graph ####
NPP_Join_Alive$treatment<-gsub("-"," ", NPP_Join_Alive$treatment)

ANPP_Graph <- ggplot(NPP_Join_Alive, aes(x = treatment, y = alive_ANPP_g, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Alive ANPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,2.5))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  annotate("text", x=0.6, y=2.5, label = "D.", size=20)

#### Figure 2E: BNPP Graph ####
BNPP_Graph <- ggplot(NPP_Join_Alive, aes(x = treatment, y = BNPP_g, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="BNPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,2.5))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  annotate("text", x=0.6, y=2.5, label = "E.", size=20)+
  annotate("text", x=1, y=1.7, label = "a", size=20)+
  annotate("text", x=2, y=1.7, label = "b", size=20)+
  annotate("text", x=3, y=1.7, label = "ab", size=20)+
  annotate("text", x=4, y=1.7, label = "ab", size=20)

#### Figure 2F: NPP Figure ####
ANPP_BNPP_Graph <- ggplot(NPP_Join_Alive, aes(x = treatment, y = ANPP_BNPP_ratio, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Alive ANPP:BNPP") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,2.5))+
  geom_hline(yintercept = 1, colour = "gray20",size=1,linetype="dashed")+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  annotate("text", x=0.6, y=2.5, label = "F.", size=20)

#### Create 6 paneled Figure 2 ####
MaxLL_Graph+
  Leaf_Num_Graph+
  MaxLL_GR_Graph+
  ANPP_Graph+
  BNPP_Graph+
  ANPP_BNPP_Graph+
  plot_layout(ncol = 3,nrow = 2)
#save at 3500 x 2250

#### Trait Stats ####

#### SLA Stats  ####

# check for normality #
#non transformed data
Normality_test_SLA <- lm(data = Leaf_Data_Join, SLA  ~ treatment)
ols_plot_resid_hist(Normality_test_SLA) 
ols_test_normality(Normality_test_SLA)
#transform data
Leaf_Data_Join<-Leaf_Data_Join %>% 
  mutate(SLA_TF=log(SLA))
#check normality of transformed data
Normality_test_SLA_TF <- lm(data = Leaf_Data_Join, SLA_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_SLA_TF) 
ols_test_normality(Normality_test_SLA_TF)#best transformed with log 

#check for homoscedascity
leveneTest(SLA_TF ~ treatment, data = Leaf_Data_Join) #p = 0.2044 so > 0.05 so equal variance is met

#run model 
SLA_model <- aov(SLA_TF ~ treatment, data = Leaf_Data_Join)
summary(SLA_model) #p=0.000274
summary(glht(SLA_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### LDMC Stats ####

# check for normality #
#non transformed data
Normality_test_LDMC <- lm(data = Leaf_Data_Join, LDMC  ~ treatment)
ols_plot_resid_hist(Normality_test_LDMC) 
ols_test_normality(Normality_test_LDMC)
#transform data
Leaf_Data_Join<-Leaf_Data_Join %>% 
  mutate(LDMC_TF=log10(LDMC))
#check normality of transformed data
Normality_test_LDMC_TF <- lm(data = Leaf_Data_Join, LDMC_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_LDMC_TF) 
ols_test_normality(Normality_test_LDMC_TF)#best transformed with log

#check for homoscedascity
leveneTest(LDMC_TF ~ treatment, data = Leaf_Data_Join) #p = 0.08486 so < 0.05 so equal variance is not met 

#run model 
LDMC_model <- aov(LDMC_TF ~ treatment, data = Leaf_Data_Join)
summary(LDMC_model) #p=4.17e-06
summary(glht(LDMC_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### Leaf Thickness Stats ####

# check for normality #
#non transformed data
Normality_test_leaf_thickness <- lm(data = Leaf_Data_Join, leaf_thickness  ~ treatment)
ols_plot_resid_hist(Normality_test_leaf_thickness) 
ols_test_normality(Normality_test_leaf_thickness)
#transform data
Leaf_Data_Join<-Leaf_Data_Join %>% 
  mutate(leaf_thickness_TF=log10(leaf_thickness))
#check normality of transformed data
Normality_test_leaf_thickness_TF <- lm(data = Leaf_Data_Join, leaf_thickness_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_leaf_thickness_TF) 
ols_test_normality(Normality_test_leaf_thickness_TF)#best transformed with log

#check for homoscedascity
leveneTest(leaf_thickness_TF ~ treatment, data = Leaf_Data_Join) #p = 0.04143 so < 0.05 so equal variance is not met 

#run model 
leaf_thickness_model <- aov(leaf_thickness_TF ~ treatment, data = Leaf_Data_Join)
summary(leaf_thickness_model) #p=1.62e-05
summary(glht(leaf_thickness_model, linfct = mcp(treatment = "Tukey")), test = adjusted(type = "BH"))

#### Figure 4: Traits Figure ####
Leaf_Data_Join$treatment<-gsub("-"," ", Leaf_Data_Join$treatment)

#### Figure 4A: SLA Graph ####
SLA_Graph <- ggplot(Leaf_Data_Join, aes(x = treatment, y = SLA, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y =expression ("Specific Leaf Area"~(mm^2/g))) +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=1000)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  annotate("text", x=0.6, y=1000, label = "A.", size=20)+
  annotate("text", x=1, y=900, label = "a", size=20)+
  annotate("text", x=2, y=900, label = "b", size=20)+
  annotate("text", x=3, y=900, label = "b", size=20)+
  annotate("text", x=4, y=900, label = "b", size=20)

#### Figure 4B: LDMC Graph ####
LDMC_Graph <- ggplot(Leaf_Data_Join, aes(x = treatment, y = LDMC, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Dry Matter Content (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=2)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  annotate("text", x=0.6, y=2, label = "B.", size=20)+
  annotate("text", x=1, y=1.75, label = "a", size=20)+
  annotate("text", x=2, y=1.75, label = "b", size=20)+
  annotate("text", x=3, y=1.75, label = "b", size=20)+
  annotate("text", x=4, y=1.75, label = "b", size=20)

#### Figure 4C: Leaf Thickness Graph ####
LeafThickness_Graph <- ggplot(Leaf_Data_Join, aes(x = treatment, y = leaf_thickness, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Thickness (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=0.5)+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  annotate("text", x=0.6, y=0.5, label = "C.", size=20)+
  annotate("text", x=1, y=0.42, label = "a", size=20)+
  annotate("text", x=2, y=0.42, label = "b", size=20)+
  annotate("text", x=3, y=0.42, label = "b", size=20)+
  annotate("text", x=4, y=0.42, label = "a", size=20)

#### Create Figure 3 ####
SLA_Graph+
  LDMC_Graph+
  LeafThickness_Graph+
  plot_layout(ncol = 1,nrow = 3)
#save at 2000 x 3000

#### Figure 5: Restoration Stats ####

#### Fuel Load Stats ####

# check for normality #
#non transformed data
Normality_test_TotalANPP_Join <- lm(data = NPP_Join, total_ANPP_g  ~ treatment)
ols_plot_resid_hist(Normality_test_TotalANPP_Join) 
ols_test_normality(Normality_test_TotalANPP_Join)
#transform data
NPP_Join<-NPP_Join %>% 
  mutate(total_ANPP_g_TF=sqrt(total_ANPP_g))
#check normality of transformed data
Normality_test_TotalANPP_Join_TF <- lm(data = NPP_Join, total_ANPP_g_TF  ~ treatment)
ols_plot_resid_hist(Normality_test_TotalANPP_Join_TF) 
ols_test_normality(Normality_test_TotalANPP_Join_TF)#best transformed with sqrt

#check for homoscedascity
leveneTest(total_ANPP_g_TF ~ treatment, data = NPP_Join) #p = 0.02268 so < 0.05 so equal variance is met 

#run model 
TotalANPP_model <- aov(total_ANPP_g_TF ~ treatment, data = NPP_Join)
summary(TotalANPP_model) #p=0.114

#### Figure 5: Restoration Figure ####

#### Figure 5A: Survival ####
End_Time_Point_A_D$treatment<-gsub("-"," ", End_Time_Point_A_D$treatment)
#stacked bar graph of final alive and dead by treatment - WITH percentages on bars
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #palette

Survival_Graph<-ggplot(data=End_Time_Point_A_D %>%
                   count(treatment, survival) %>% #gets counts of unique observations
                   group_by(treatment) %>% # groups by treatment
                   mutate(percent=n/sum(n)), # finds percentage of alive and dead within each treatment to use for percentage labels
                 aes(treatment, n, fill=survival)) +
  geom_bar(stat="identity") +
  labs(x = "Treatment",y ="Number of Plants") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")), #percentage label format
            position=position_stack(vjust=0.5),
            size=18) +
  expand_limits(y=c(0,45))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(legend.title=element_blank(),legend.position = c(0.8,0.9),legend.key = element_rect(size=20), legend.key.size = unit(5.0, 'lines'),axis.title.x=element_blank(), axis.text.x = element_blank()) + #remove legend title
  scale_fill_manual(values=c(cbPalette[3],cbPalette[7]),labels=c('Alive Plants', 'Dead Plants'))+ #set colors
  annotate("text", x=0.6, y=45, label = "A.", size=20)
#save at 2000x20000

#### ## Figure 5c. Fuel Load Graph ####
NPP_Join$treatment<-gsub("-"," ", NPP_Join$treatment)
Fuel_Load_Graph<-ggplot(NPP_Join, aes(x = treatment, y = total_ANPP_g, fill= treatment)) +
  geom_boxplot(outlier.size=4,lwd=1) +
  #create axis labels
  labs(x = "Treatment",y ="Fuel Load (g of aboveground biomass)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=c(0,3))+
  #change color of treatments
  scale_fill_manual(values=c( "#76AFE8","#88A76E","#E6E291","#CA7E77"))+
  #wrap text for x axis ticks using stringr package
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  annotate("text", x=0.6, y=3, label = "B.", size=20)
#save at 2200x2000


#Create Figure
Survival_Graph +
  Fuel_Load_Graph +
  plot_layout(ncol = 1,nrow = 2)
#save at 1500 x 3000
