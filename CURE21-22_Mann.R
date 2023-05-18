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
Through_Time <- read.csv("ALL_llp315cure_499data.csv", header = TRUE, na.strings = "", colClasses = c("character", "character", "character", "character","character", "numeric", "factor", "factor", "factor", "factor", "factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")) %>% 
  select(-c(start_time,end_time))

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
#### Don't- this messes up the dataframes for me. Instead, continue to the endpoint data ####
Through_Time_Join = Through_Time_Join[!(Through_Time_Join$week_num < 9 & Through_Time_Join$biomass_removed > 0), ] #For me, it removes all the biomass removed with values greater than 0, not just in week 9, but in all weeks. That is why I have removed it. I would ask someone to look over my data, as it makes a very big difference


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

Linear Hypotheses:
                                          Estimate  Std. Error  t value Pr(>|t|)  
Control-Heatwave - Control-Control == 0    -35.517     23.146  -1.534   0.2549  
Heatwave-Control - Control-Control == 0    -62.250     22.964  -2.711   0.0460 *
Heatwave-Heatwave - Control-Control == 0   -53.908     23.754  -2.269   0.0749 .
Heatwave-Control - Control-Heatwave == 0   -26.734     23.315  -1.147   0.3806  
Heatwave-Heatwave - Control-Heatwave == 0  -18.391     24.094  -0.763   0.5361  
Heatwave-Heatwave - Heatwave-Control == 0    8.343     23.920   0.349   0.7278  


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

Linear Hypotheses:
  Estimate Std. Error t value Pr(>|t|)   
Control-Heatwave - Control-Control == 0    -33.357     10.688  -3.121  0.00614 **
Heatwave-Control - Control-Control == 0    -13.112     10.593  -1.238  0.26059   
Heatwave-Heatwave - Control-Control == 0   -38.168     10.790  -3.538  0.00296 **
Heatwave-Control - Control-Heatwave == 0    20.246     10.642   1.902  0.08765 . 
Heatwave-Heatwave - Control-Heatwave == 0   -4.811     10.837  -0.444  0.65751   
Heatwave-Heatwave - Heatwave-Control == 0  -25.057     10.743  -2.332  0.04120 * 



#run model not using any plants that had biomass removed
MaxPH_model_noCG <- aov(max_plant_height ~ overall_group, data = End_Time_Point_CGRemoval)
summary(MaxPH_model_noCG) #p=0.0131

#Post-Hoc
summary(glht(MaxPH_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                         Estimate   Std. Error  t value  Pr(>|t|)  
Control-Heatwave - Control-Control == 0    -43.993     15.023  -2.928   0.0228 *
Heatwave-Control - Control-Control == 0    -20.360     14.906  -1.366   0.2093  
Heatwave-Heatwave - Control-Control == 0   -41.842     15.418  -2.714   0.0228 *
Heatwave-Control - Control-Heatwave == 0    23.633     15.133   1.562   0.2093  
Heatwave-Heatwave - Control-Heatwave == 0    2.151     15.639   0.138   0.8908  
Heatwave-Heatwave - Heatwave-Control == 0  -21.482     15.526  -1.384   0.2093  


# run model accounting for biomass removed
MaxPH_model_biomass <- lmerTest::lmer(max_plant_height ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(MaxPH_model_biomass) #p=0.001194

summary(glht(MaxPH_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                          Estimate  Std. Error z value  Pr(>|z|)   
Control-Heatwave - Control-Control == 0    -33.340     10.680  -3.122  0.00539 **
Heatwave-Control - Control-Control == 0    -13.055     10.587  -1.233  0.26103   
Heatwave-Heatwave - Control-Control == 0   -38.046     10.783  -3.528  0.00251 **
Heatwave-Control - Control-Heatwave == 0    20.285     10.634   1.908  0.08467 . 
Heatwave-Heatwave - Control-Heatwave == 0   -4.705     10.831  -0.434  0.66399   
Heatwave-Heatwave - Heatwave-Control == 0  -24.991     10.736  -2.328  0.03985 * 

  

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


summary(glht(leaf_num_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate  Std. Error  t value   Pr(>|t|)    
  Control-Heatwave - Control-Control == 0    -12.564      1.922  -6.538 2.64e-09 ***
  Heatwave-Control - Control-Control == 0     -4.123      1.905  -2.165   0.0378 *  
  Heatwave-Heatwave - Control-Control == 0    -8.113      1.940  -4.182 8.37e-05 ***
  Heatwave-Control - Control-Heatwave == 0     8.441      1.913   4.412 4.85e-05 ***
  Heatwave-Heatwave - Control-Heatwave == 0    4.450      1.948   2.284   0.0350 *  
  Heatwave-Heatwave - Heatwave-Control == 0   -3.990      1.932  -2.066   0.0400 *  



#run model not using any plants that had biomass removed
leaf_num_model_noCG <- aov(leaf_num ~ overall_group, data = End_Time_Point_CGRemoval)
summary(leaf_num_model_noCG) #p=2.48e-09

summary(glht(leaf_num_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))


Linear Hypotheses:
                                            Estimate  Std. Error t value  Pr(>|t|)    
  Control-Heatwave - Control-Control == 0    -17.649      2.489  -7.090 5.38e-10 ***
  Heatwave-Control - Control-Control == 0     -6.906      2.470  -2.796 0.007197 ** 
  Heatwave-Heatwave - Control-Control == 0    -9.152      2.555  -3.582 0.000975 ***
  Heatwave-Control - Control-Heatwave == 0    10.743      2.507   4.285 0.000109 ***
  Heatwave-Heatwave - Control-Heatwave == 0    8.497      2.591   3.279 0.002028 ** 
  Heatwave-Heatwave - Heatwave-Control == 0   -2.247      2.572  -0.873 0.384172    


# run model accounting for biomass removed
leaf_num_model_biomass <- lmerTest::lmer(leaf_num ~ overall_group + (1 | biomass_removed), data = End_Time_Point)
anova(leaf_num_model_biomass) #p=2.112e-09

summary(glht(leaf_num_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                              Estimate  Std. Error z value  Pr(>|z|)    
  Control-Heatwave - Control-Control == 0    -12.622      1.911  -6.604 2.40e-10 ***
  Heatwave-Control - Control-Control == 0     -4.124      1.897  -2.174   0.0356 *  
  Heatwave-Heatwave - Control-Control == 0    -8.059      1.931  -4.174 5.99e-05 ***
  Heatwave-Control - Control-Heatwave == 0     8.498      1.904   4.463 2.43e-05 ***
  Heatwave-Heatwave - Control-Heatwave == 0    4.563      1.941   2.351   0.0281 *  
  Heatwave-Heatwave - Heatwave-Control == 0   -3.935      1.922  -2.047   0.0406 *  


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

summary(glht(alive_ANPP_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                         Estimate  Std. Error t value   Pr(>|t|)    
Control-Heatwave - Control-Control == 0   -0.25863    0.06657  -3.885  0.00085 ***
Heatwave-Control - Control-Control == 0   -0.10804    0.06453  -1.674  0.11493    
Heatwave-Heatwave - Control-Control == 0  -0.22996    0.06549  -3.511  0.00168 ** 
Heatwave-Control - Control-Heatwave == 0   0.15059    0.06528   2.307  0.04430 *  
Heatwave-Heatwave - Control-Heatwave == 0  0.02868    0.06623   0.433  0.66552    
Heatwave-Heatwave - Heatwave-Control == 0 -0.12192    0.06418  -1.900  0.08856 .


#run model not using any plants that had biomass removed
alive_ANPP_model_noCG <- aov(alive_ANPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(alive_ANPP_model_noCG) #0.00767

summary(glht(alive_ANPP_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                         Estimate  Std. Error  t value   Pr(>|t|)   
Control-Heatwave - Control-Control == 0   -0.26394    0.09524  -2.771  0.01984 * 
Heatwave-Control - Control-Control == 0   -0.16223    0.09015  -1.800  0.14966   
Heatwave-Heatwave - Control-Control == 0  -0.29973    0.09163  -3.271  0.00873 **
Heatwave-Control - Control-Heatwave == 0   0.10171    0.09214   1.104  0.32663   
Heatwave-Heatwave - Control-Heatwave == 0 -0.03578    0.09359  -0.382  0.70301   
Heatwave-Heatwave - Heatwave-Control == 0 -0.13749    0.08841  -1.555  0.18440


# run model accounting for biomass removed
alive_ANPP_model_biomass <- lmerTest::lmer(alive_ANPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(alive_ANPP_model_biomass) #0.0003199

summary(glht(alive_ANPP_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate   Std. Error  z value Pr(>|z|)    
Control-Heatwave - Control-Control == 0   -0.25803    0.06638  -3.887 0.000609 ***
Heatwave-Control - Control-Control == 0   -0.10951    0.06438  -1.701 0.106740    
Heatwave-Heatwave - Control-Control == 0  -0.23126    0.06530  -3.541 0.001195 ** 
Heatwave-Control - Control-Heatwave == 0   0.14852    0.06512   2.281 0.045116 *  
Heatwave-Heatwave - Control-Heatwave == 0  0.02677    0.06607   0.405 0.685337    
Heatwave-Heatwave - Heatwave-Control == 0 -0.12175    0.06401  -1.902 0.085772 .  

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

summary(glht(dead_ANPP_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                          Estimate  Std. Error  t value Pr(>|t|)  
Control-Heatwave - Control-Control == 0    0.04619    0.05748   0.804   0.5072  
Heatwave-Control - Control-Control == 0   -0.02633    0.05540  -0.475   0.6351  
Heatwave-Heatwave - Control-Control == 0   0.11987    0.05622   2.132   0.1030  
Heatwave-Control - Control-Heatwave == 0  -0.07252    0.05637  -1.286   0.2998  
Heatwave-Heatwave - Control-Heatwave == 0  0.07369    0.05718   1.289   0.2998  
Heatwave-Heatwave - Heatwave-Control == 0  0.14621    0.05510   2.654   0.0519 .

#run model not using any plants that had biomass removed
dead_ANPP_model_noCG <- aov(dead_ANPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(dead_ANPP_model_noCG) #0.0271

summary(glht(dead_ANPP_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                          Estimate  Std. Error  t value Pr(>|t|)  
Control-Heatwave - Control-Control == 0    0.08580    0.07734   1.109   0.4048  
Heatwave-Control - Control-Control == 0   -0.11701    0.07239  -1.616   0.2182  
Heatwave-Heatwave - Control-Control == 0   0.06467    0.07358   0.879   0.4578  
Heatwave-Control - Control-Heatwave == 0  -0.20280    0.07488  -2.708   0.0358 *
Heatwave-Heatwave - Control-Heatwave == 0 -0.02112    0.07603  -0.278   0.7817  
Heatwave-Heatwave - Heatwave-Control == 0  0.18168    0.07099   2.559   0.0358 *

# run model accounting for biomass removed
dead_ANPP_model_biomass <- lmerTest::lmer(dead_ANPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(dead_ANPP_model_biomass) #0.01366

summary(glht(dead_ANPP_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate    Std. Error  z value Pr(>|z|)  
Control-Heatwave - Control-Control == 0    0.05619    0.05049   1.113   0.3640  
Heatwave-Control - Control-Control == 0   -0.04261    0.04878  -0.874   0.3824  
Heatwave-Heatwave - Control-Control == 0   0.10803    0.04929   2.192   0.0852 .
Heatwave-Control - Control-Heatwave == 0  -0.09880    0.04959  -1.992   0.0926 .
Heatwave-Heatwave - Control-Heatwave == 0  0.05183    0.05036   1.029   0.3640  
Heatwave-Heatwave - Heatwave-Control == 0  0.15064    0.04846   3.109   0.0113 *


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

summary(glht(total_ANPP_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate    Std. Error  t value Pr(>|t|)  
Control-Heatwave - Control-Control == 0   -0.22136    0.08767  -2.525   0.0744 .
Heatwave-Control - Control-Control == 0   -0.13437    0.08499  -1.581   0.3054  
Heatwave-Heatwave - Control-Control == 0  -0.11009    0.08626  -1.276   0.3054  
Heatwave-Control - Control-Heatwave == 0   0.08699    0.08597   1.012   0.3755  
Heatwave-Heatwave - Control-Heatwave == 0  0.11128    0.08722   1.276   0.3054  
Heatwave-Heatwave - Heatwave-Control == 0  0.02429    0.08453   0.287   0.7742 

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

summary(glht(BNPP_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate    Std. Error t value Pr(>|t|)   
Control-Heatwave - Control-Control == 0   -0.08989    0.05052  -1.779  0.15361   
Heatwave-Control - Control-Control == 0   -0.16339    0.04945  -3.304  0.00687 **
Heatwave-Heatwave - Control-Control == 0  -0.04543    0.04970  -0.914  0.37751   
Heatwave-Control - Control-Heatwave == 0  -0.07350    0.05001  -1.470  0.21503   
Heatwave-Heatwave - Control-Heatwave == 0  0.04446    0.05026   0.885  0.37751   
Heatwave-Heatwave - Heatwave-Control == 0  0.11796    0.04919   2.398  0.05243 . 

#run model not using any plants that had biomass removed
BNPP_model_noCG <- aov(BNPP_g~ overall_group, data = NPP_Join_CGRemoval)
summary(BNPP_model_noCG) #0.0119

summary(glht(BNPP_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate    Std. Error t value  Pr(>|t|)  
Control-Heatwave - Control-Control == 0   -0.03988    0.06361  -0.627   0.6385  
Heatwave-Control - Control-Control == 0   -0.19323    0.06069  -3.184   0.0115 *
Heatwave-Heatwave - Control-Control == 0  -0.06757    0.06120  -1.104   0.4082  
Heatwave-Control - Control-Heatwave == 0  -0.15335    0.06201  -2.473   0.0451 *
Heatwave-Heatwave - Control-Heatwave == 0 -0.02769    0.06251  -0.443   0.6587  
Heatwave-Heatwave - Heatwave-Control == 0  0.12566    0.05954   2.111   0.0745 .

# run model accounting for biomass removed
BNPP_model_biomass <- lmerTest::lmer(BNPP_g ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(BNPP_model_biomass) #0.007883

summary(glht(BNPP_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate    Std. Error z value Pr(>|z|)   
Control-Heatwave - Control-Control == 0   -0.08758    0.04980  -1.759  0.15721   
Heatwave-Control - Control-Control == 0   -0.16439    0.04891  -3.361  0.00466 **
Heatwave-Heatwave - Control-Control == 0  -0.04622    0.04890  -0.945  0.40512   
Heatwave-Control - Control-Heatwave == 0  -0.07680    0.04945  -1.553  0.18056   
Heatwave-Heatwave - Control-Heatwave == 0  0.04136    0.04968   0.833  0.40512   
Heatwave-Heatwave - Heatwave-Control == 0  0.11817    0.04858   2.432  0.04500 * 

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

summary(glht(NPP_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                        Estimate Std. Error t value Pr(>|t|)  
Control-Heatwave - Control-Control == 0   -0.311254   0.112775  -2.760   0.0196 *
Heatwave-Control - Control-Control == 0   -0.303747   0.110398  -2.751   0.0196 *
Heatwave-Heatwave - Control-Control == 0  -0.155518   0.110960  -1.402   0.2144  
Heatwave-Control - Control-Heatwave == 0   0.007507   0.111643   0.067   0.9465  
Heatwave-Heatwave - Control-Heatwave == 0  0.155736   0.112199   1.388   0.2144  
Heatwave-Heatwave - Heatwave-Control == 0  0.148229   0.109810   1.350   0.2144  

#run model not using any plants that had biomass removed
NPP_model_noCG <- aov(NPP ~ overall_group, data = NPP_Join_CGRemoval)
summary(NPP_model_noCG) #0.0101

summary(glht(NPP_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                          Estimate Std. Error t value Pr(>|t|)   
Control-Heatwave - Control-Control == 0   -0.24295    0.14826  -1.639  0.15648   
Heatwave-Control - Control-Control == 0   -0.48408    0.14146  -3.422  0.00535 **
Heatwave-Heatwave - Control-Control == 0  -0.30263    0.14264  -2.122  0.10883   
Heatwave-Control - Control-Heatwave == 0  -0.24113    0.14453  -1.668  0.15648   
Heatwave-Heatwave - Control-Heatwave == 0 -0.05967    0.14569  -0.410  0.68298   
Heatwave-Heatwave - Heatwave-Control == 0  0.18146    0.13877   1.308  0.23269   

# run model accounting for biomass removed
NPP_model_biomass <- lmerTest::lmer(NPP ~ overall_group + (1 | biomass_removed), data = NPP_Join)
anova(NPP_model_biomass) #0.009993

summary(glht(NPP_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate Std. Error z value Pr(>|z|)  
Control-Heatwave - Control-Control == 0   -0.30298    0.10834  -2.797   0.0155 *
Heatwave-Control - Control-Control == 0   -0.32451    0.10630  -3.053   0.0136 *
Heatwave-Heatwave - Control-Control == 0  -0.17054    0.10646  -1.602   0.2173  
Heatwave-Control - Control-Heatwave == 0  -0.02153    0.10749  -0.200   0.8412  
Heatwave-Heatwave - Control-Heatwave == 0  0.13244    0.10801   1.226   0.2641  
Heatwave-Heatwave - Heatwave-Control == 0  0.15398    0.10562   1.458   0.2173  

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

summary(glht(SLA_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate Std. Error t value Pr(>|t|)    
Control-Heatwave - Control-Control == 0    -50.860     12.795  -3.975  0.00026 ***
Heatwave-Control - Control-Control == 0    -40.919     11.796  -3.469  0.00118 ** 
Heatwave-Heatwave - Control-Control == 0   -58.099     12.344  -4.707 2.23e-05 ***
Heatwave-Control - Control-Heatwave == 0     9.941     12.525   0.794  0.51353    
Heatwave-Heatwave - Control-Heatwave == 0   -7.239     13.043  -0.555  0.57926    
Heatwave-Heatwave - Heatwave-Control == 0  -17.180     12.064  -1.424  0.23305 


#run model not using any plants that had biomass removed
SLA_model_noCG <- aov(SLA ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(SLA_model_noCG) #p=0.00234

summary(glht(SLA_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                           Estimate Std. Error t value Pr(>|t|)   
Control-Heatwave - Control-Control == 0     -68.50      20.66  -3.316  0.00394 **
Heatwave-Control - Control-Control == 0     -41.42      18.14  -2.283  0.04735 * 
Heatwave-Heatwave - Control-Control == 0    -63.09      19.31  -3.267  0.00394 **
Heatwave-Control - Control-Heatwave == 0     27.08      20.10   1.348  0.26933   
Heatwave-Heatwave - Control-Heatwave == 0     5.41      21.16   0.256  0.79852   
Heatwave-Heatwave - Heatwave-Control == 0   -21.68      18.71  -1.158  0.29805 

# run model accounting for biomass removed
SLA_model_biomass <- lmerTest::lmer(SLA ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(SLA_model_biomass) #p=8.0788e-06

summary(glht(SLA_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                           Estimate Std. Error z value Pr(>|z|)    
Control-Heatwave - Control-Control == 0    -51.961     12.815  -4.055 0.000151 ***
Heatwave-Control - Control-Control == 0    -40.748     11.814  -3.449 0.001125 ** 
Heatwave-Heatwave - Control-Control == 0   -58.498     12.350  -4.737  1.3e-05 ***
Heatwave-Control - Control-Heatwave == 0    11.213     12.553   0.893 0.446036    
Heatwave-Heatwave - Control-Heatwave == 0   -6.537     13.079  -0.500 0.617218    
Heatwave-Heatwave - Heatwave-Control == 0  -17.750     12.086  -1.469 0.212900 



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

summary(glht(Thickness_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate Std. Error t value Pr(>|t|)    
Control-Heatwave - Control-Control == 0   -0.045712   0.007566  -6.042 1.24e-08 ***
Heatwave-Control - Control-Control == 0   -0.051626   0.006930  -7.449 5.00e-12 ***
Heatwave-Heatwave - Control-Control == 0  -0.019770   0.007270  -2.719  0.00827 ** 
Heatwave-Control - Control-Heatwave == 0  -0.005914   0.007391  -0.800  0.42425    
Heatwave-Heatwave - Control-Heatwave == 0  0.025942   0.007711   3.364  0.00129 ** 
Heatwave-Heatwave - Heatwave-Control == 0  0.031856   0.007088   4.494 1.94e-05 ***


#run model not using any plants that had biomass removed
Thickness_model_noCG <- aov(leaf_thickness ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(Thickness_model_noCG) #p=1.32e-05

summary(glht(Thickness_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate Std. Error t value Pr(>|t|)    
Control-Heatwave - Control-Control == 0   -0.029890   0.010133  -2.950 0.005430 ** 
Heatwave-Control - Control-Control == 0   -0.035466   0.008863  -4.002 0.000279 ***
Heatwave-Heatwave - Control-Control == 0   0.002837   0.009474   0.299 0.764958    
Heatwave-Control - Control-Heatwave == 0  -0.005576   0.009824  -0.568 0.685294    
Heatwave-Heatwave - Control-Heatwave == 0  0.032727   0.010379   3.153 0.003807 ** 
Heatwave-Heatwave - Heatwave-Control == 0  0.038303   0.009143   4.189 0.000267 ***

# run model accounting for biomass removed
Thickness_model_biomass <- lmerTest::lmer(leaf_thickness ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(Thickness_model_biomass) #p=2.588e-11

summary(glht(Thickness_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            Estimate Std. Error z value Pr(>|z|)    
Control-Heatwave - Control-Control == 0   -0.042877   0.007606  -5.637 5.18e-08 ***
Heatwave-Control - Control-Control == 0   -0.046448   0.006995  -6.640 1.88e-10 ***
Heatwave-Heatwave - Control-Control == 0  -0.014931   0.007272  -2.053 0.048069 *  
Heatwave-Control - Control-Heatwave == 0  -0.003571   0.007439  -0.480 0.631198    
Heatwave-Heatwave - Control-Heatwave == 0  0.027946   0.007886   3.544 0.000591 ***
Heatwave-Heatwave - Heatwave-Control == 0  0.031517   0.007197   4.379 2.38e-05 ***





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

summary(glht(LDMC_model, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                           Estimate Std. Error t value Pr(>|t|)    
Control-Heatwave - Control-Control == 0    0.20038    0.03950   5.072 3.95e-06 ***
Heatwave-Control - Control-Control == 0    0.09592    0.03642   2.634   0.0133 *  
Heatwave-Heatwave - Control-Control == 0   0.18429    0.03811   4.836 6.13e-06 ***
Heatwave-Control - Control-Heatwave == 0  -0.10446    0.03867  -2.701   0.0133 *  
Heatwave-Heatwave - Control-Heatwave == 0 -0.01609    0.04027  -0.400   0.6898    
Heatwave-Heatwave - Heatwave-Control == 0  0.08837    0.03725   2.373   0.0219 *  


#run model not using any plants that had biomass removed
LDMC_model_noCG <- aov(LDMC ~ overall_group, data = Leaf_Data_Join_CGRemoval)
summary(LDMC_model_noCG) #p=1.03e-07

summary(glht(LDMC_model_noCG, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                           Estimate Std. Error t value Pr(>|t|)    
Control-Heatwave - Control-Control == 0    0.20822    0.03977   5.236 1.42e-06 ***
Heatwave-Control - Control-Control == 0    0.10419    0.03493   2.983  0.00654 ** 
Heatwave-Heatwave - Control-Control == 0   0.19751    0.03718   5.313 1.42e-06 ***
Heatwave-Control - Control-Heatwave == 0  -0.10403    0.03869  -2.689  0.01181 *  
Heatwave-Heatwave - Control-Heatwave == 0 -0.01071    0.04073  -0.263  0.79294    
Heatwave-Heatwave - Heatwave-Control == 0  0.09332    0.03602   2.591  0.01249 *  

# run model accounting for biomass removed
LDMC_model_biomass <- lmerTest::lmer(LDMC ~ overall_group + (1 | biomass_removed), data = Leaf_Data_Join)
anova(LDMC_model_biomass) #p=1.433e-08

summary(glht(LDMC_model_biomass, linfct = mcp(overall_group = "Tukey")), test = adjusted(type = "BH"))

Linear Hypotheses:
                                            SEstimate Std. Error z value Pr(>|z|)    
Control-Heatwave - Control-Control == 0    0.21113    0.03856   5.476 2.61e-07 ***
Heatwave-Control - Control-Control == 0    0.09394    0.03580   2.624  0.01042 *  
Heatwave-Heatwave - Control-Control == 0   0.19452    0.03702   5.254 4.45e-07 ***
Heatwave-Control - Control-Heatwave == 0  -0.11719    0.03797  -3.087  0.00405 ** 
Heatwave-Heatwave - Control-Heatwave == 0 -0.01661    0.04039  -0.411  0.68086    
Heatwave-Heatwave - Heatwave-Control == 0  0.10058    0.03698   2.720  0.00980 ** 


