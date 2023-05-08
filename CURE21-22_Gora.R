#### Master Script for CURE 21-22 Manuscript ####
#Sarah Gora Script




#### Load in packages ####
library(githubinstall)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
library(ggplot2)
library(tidyverse)
#set colorblind friendly color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Set working directory ####
setwd("~/Desktop/Cure_Project")


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


# read in code for bringing in and cleaning invaded pot biomass data:

#read in data on pots that had biomass removed early season due to crabgrass invasion or incorrect species
biomass_removed_early <- read.csv("removed_biomass.csv", header = TRUE, na.strings = "", colClasses = c("factor", "factor", "numeric", "factor")) %>%
  separate(fall_plant_ID, c("fall_day", "fall_treatment", "fall_plant"), sep = "_") %>%
  select(fall_plant, overall_group, biomass_removed)

#### Clean Up Biomass Removed Data ####
biomass_removed_early$fall_plant <- gsub("p","P",biomass_removed_early$fall_plant)
biomass_removed_early$fall_plant <- gsub(" P","P",biomass_removed_early$fall_plant)

# this uses "fall_plant" as the ID that you can match up to other dataframes, so it needs "fall_plant" to be included even though those aren't the final IDs



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
  select(c(overall_group,spring_plant_ID,fall_plant_ID,date,week_num,tray_ID,survival,max_leaf_length,max_plant_height,leaf_num,soil_moisture,light_avail,air_temp,humidity)) #### some things have NAs here -- look at data to find problem ####

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
  select(overall_group,spring_plant_ID,leaf_number,wet_leaf_weight,dry_leaf_weight,leaf_area,leaf_thickness,fall_plant) %>% 
  mutate(LDMC = dry_leaf_weight / wet_leaf_weight) %>%
  mutate(SLA = leaf_area / dry_leaf_weight) 



#############  SG Code Starts here, 5/8/2023

##### Merge, Removed biomass data 

# load in the Removed Crab data
Removed_Crab <- read_csv("removed_biomass.csv")
View(Removed_Crab)

# Remove the plants from Leaf Data Data
# 185, 190, 193

# full join Removed Biomass Data with Leaf_Data_Join data
Leaf_Data_Crab_Join <- Leaf_Data_Join %>%
  full_join(biomass_removed_early)


# For Biomass column, change NA to 0 
Leaf_Data_Crab_Join$biomass_removed[is.na(Leaf_Data_Crab_Join$biomass_removed)] <- 0
View(Leaf_Data_Crab_Join)

# Remove NAs 
Leaf_Data_Crab_Join_RMVCrab <- na.omit(Leaf_Data_Crab_Join)
View(Leaf_Data_Crab_Join_RMVCrab)

######### SG Gameplan ###########

# run packages 
library(lme4)

### DATA
# Cleaned up, End time Point, Week 22 
View(Leaf_Data_Crab_Join)

### 4 TRAITS
# SLA
# LDMC
# Leaf Thickness 
# Plant Stress (use count data, not averages- data visualization, no stats)


######## RUN 3 MODELS for each Trait
### 1. lmer model: Week 22- allplants 
###           lmer(SLA ~ treatment + (1|crabgrass))
### 2. lmer model: Week 22- allplants 
###           lmer(SLA ~ treatment)
### 3. aov model: Week 22- New DF, allplants-remved crabbgrass 
###           aov(SLA ~ treatment + (1 | crabgrass))


######## RUN AIC Score
### Pick model with lowest AIC score 

######## Make Boxplot with ggplot theme




############################################################

### TRAIT 1: SLA 

# run lmer 1
mSLA1 <- lmer(SLA ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Crab_Join)
summary(mSLA1)
# mSLA1 <- summary(mSLA1)
# capture.output(mSLA1, file = "Gora_CURE_Table1-GLMM.txt")

# run aov 2
mSLA2 <- aov(SLA ~ overall_group, data = Leaf_Data_Crab_Join)
summary(mSLA2)

# run aov 3- on data with plants with crabgrass removed
mSLA3 <- aov(SLA ~ overall_group, data = Leaf_Data_Crab_Join_RMVCrab)
summary(mSLA3)

### run AIC
AIC(mSLA1) 
AIC(mSLA2)
AIC(mSLA3)  # *** lowest aka BEST model


####### Make Boxplot
### just for SG visualization- do not use this one

# subset treatment groups
Leaf_Data_CC <- which(Leaf_Data_Join$overall_group == "Control-Control")
Leaf_Data_CH <- which(Leaf_Data_Join$overall_group == "Control-Heatwave")
Leaf_Data_HH <- which(Leaf_Data_Join$overall_group == "Heatwave-Heatwave")
Leaf_Data_HC <- which(Leaf_Data_Join$overall_group == "Heatwave-Control")

# label names
cat7 <- c("Control Control", "Control Heatwave", "Heatwave Heatwave", "Heatwave Control")

# boxplot
boxplot(Leaf_Data_Join$SLA[Leaf_Data_CC], Leaf_Data_Join$SLA[Leaf_Data_CH],
        Leaf_Data_Join$SLA[Leaf_Data_HH], Leaf_Data_Join$SLA[Leaf_Data_HC],
        names= cat7,
        xlab="Treament", 
        ylab="Average SLA", 
        main= "Average SLA by Treatment", 
        pch=20)



####  SG Note- REDO boxlot in ggplot

# Had to remove NAs in Removed Biomass Data in order to graph
Leaf_Data_Crab_Join <- Leaf_Data_Join %>% full_join(biomass_removed_early) %>% filter(SLA!='NA')


# Bunch code for ggplot
ggplot(Leaf_Data_Crab_Join, aes(x = overall_group, y = SLA, fill= overall_group)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "Specific Leaf Area") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 40),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 40),
        axis.ticks.y = element_line(size = 1)) +
  guides(fill = FALSE)




############## TRAIT 2: Leaf Thickness 

# run lmer 1
mLeafTh1 <- lmer(leaf_thickness ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Crab_Join)
summary(mLeafTh1)

# run aov 2
mLeafTh2 <- aov(leaf_thickness ~ overall_group, data = Leaf_Data_Crab_Join)
summary(mLeafTh2)

# run aov 3- on data with plants with crabgrass removed
mLeafTh3 <- aov(leaf_thickness ~ overall_group, data = Leaf_Data_Crab_Join_RMVCrab)
summary(mLeafTh3)

### run AIC
AIC(mLeafTh1) 
AIC(mLeafTh2)  # *** lowest aka BEST model
AIC(mLeafTh3)  

### Remove NAs from Removed Biomass Data 
Leaf_Data_Crab_Join <- Leaf_Data_Join %>% full_join(biomass_removed_early) %>% filter(leaf_thickness!='NA')

# Make Boxplot
ggplot(Leaf_Data_Crab_Join, aes(x = overall_group, y = leaf_thickness, fill= overall_group)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "Leaf Thickness") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 40),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 40),
        axis.ticks.y = element_line(size = 1)) +
  guides(fill = FALSE)



############## TRAIT 3: LDMC

# run lmer 1
mLDMC1 <- lmer(LDMC ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Crab_Join)
summary(mLDMC1)

# run aov 2
mLDMC2 <- aov(LDMC ~ overall_group, data = Leaf_Data_Crab_Join)
summary(mLDMC2)

# run aov 3- on data with plants with crabgrass removed
mLDMC3 <- aov(LDMC ~ overall_group, data = Leaf_Data_Crab_Join_RMVCrab)
summary(mLDMC3)

### run AIC
AIC(mLDMC1) 
AIC(mLDMC2)  
AIC(mLDMC3)  # *** lowest aka BEST model

### Remove NAs from Removed Biomass Data 
Leaf_Data_Crab_Join <- Leaf_Data_Join %>% full_join(LDMC) %>% filter(leaf_thickness!='NA')

# Make Boxplot
ggplot(Leaf_Data_Crab_Join, aes(x = overall_group, y = LDMC, fill= overall_group)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "Leaf Dry Matter Content") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 40),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 40),
        axis.ticks.y = element_line(size = 1)) +
  guides(fill = FALSE)


## SG + ZB stopped here 05/8/2023


############## Trait 4: Plant Stress 

## no stats
## make figure for data visualization