#### Mann Script for CURE 21-22 Manuscript ####


#### Load in packages ####
library(githubinstall)
library(ggplot2)
library(lmerTest)
library(stringr)  
library(multcomp)
library(tidyverse)

#### Set working directory ####

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
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", colClasses = c("character", "character", "character", "character","character", "factor", "factor", "factor", "factor", "factor", "factor", "factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-fall_treatment,-spring_treatment)

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

#### Wk22 Max Leaf Length Graph ####

MaxLL_Graph <- ggplot(End_Time_Point, aes(x = overall_group, y = max_leaf_length, fill= overall_group)) +
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
MaxLL_Graph


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

MaxPH_Graph <- ggplot(End_Time_Point, aes(x = overall_group, y = max_plant_height, fill= overall_group)) +
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
MaxPH_Graph

#### Wk22 Max Plant Height Stats ####

# Run simplest model, anova comparing SLA to overall_group
MaxPH_model <- aov(max_plant_height ~ overall_group, data = End_Time_Point)
summary(MaxPH_model) #0.00117

#Post-HOC simple
summary(glht(MaxPH_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH")) 

#run model not using any plants that had biomass removed
MaxPH_model_noCG <- aov(max_plant_height ~ overall_group, data = End_Time_Point_CGRemoval)
summary(MaxPH_model_noCG) #p=0.0131

# run model accounting for biomass removed
MaxPH_model_biomass <- lmerTest::lmer(max_plant_height ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(MaxPH_model_biomass) #p=0.001194

#### Wk22 Leaf Number Graph ####

Leaf_Num_Graph <- ggplot(End_Time_Point, aes(x = overall_group, y = leaf_num, fill= overall_group)) +
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

Alive_ANPP_Graph <- ggplot(NPP_Join, aes(x = overall_group, y = alive_ANPP_g, fill= overall_group)) +
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

Dead_ANPP_Graph <- ggplot(NPP_Join, aes(x = overall_group, y = dead_ANPP_g, fill= overall_group)) +
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

ANPP_Graph <- ggplot(NPP_Join, aes(x = overall_group, y = total_ANPP_g, fill= overall_group)) +
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

BNPP_Graph <- ggplot(NPP_Join, aes(x = overall_group, y = BNPP_g, fill= overall_group)) +
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

NPP_Graph <- ggplot(NPP_Join, aes(x = overall_group, y = NPP, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Total NPP (g)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=4)+
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

SLA_Graph <- ggplot(Leaf_Data_Join, aes(x = overall_group, y = SLA, fill= overall_group)) +
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

LeafThickness_Graph <- ggplot(Leaf_Data_Join, aes(x = overall_group, y = leaf_thickness, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Thickness (mm)") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=0.5)+
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

LDMC_Graph <- ggplot(Leaf_Data_Join, aes(x = overall_group, y = LDMC, fill= overall_group)) +
  geom_boxplot() +
  #create axis labels
  labs(x = "Treatment",y ="Leaf Dry Matter Content") +
  #expand limits of graph so that the y axis goes up to 800 to encompass all points
  expand_limits(y=5)+
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
