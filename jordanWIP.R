### Jordan Winter CURE Manuscript HW

###
### Jordan: Create abiotic figures through time and run stats
###

### Graphs: there should be 4 graphs (temperature, humidity, soil moisture, light availability) -- add them to the google slides
### Stats: run an anova comparing each week (i.e. how did the treatment affect temperature for week 1, etc.)
### Don't worry about putting the stats onto the figure yet! just screenshot the results and out it in the google slides
### You can choose to start fresh on this and do it your way or I have a seperate Rscript in the github repository that you can adapt for this. 
### This was an old script and I'd prefer if you copy pieces of it to your new script because there are likely many errors/things that have since been fixed :)

###
# Load Packages
###

library(ggplot2)
library(lmerTest)
library(stringr)  
library(multcomp)
library(tidyverse)

###
# Set your own WD
###

# Push the button or code it here

###
# Set Theme
###

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

###
# Read in Data
###

df_all <- read.csv("ALL_llp315cure_499data.csv")


###
# Abiotic Temperature Figure
###

# Make a cute lil subset

TempSubset <- df_all %>% 
  select(fall_plant_ID,spring_plant_ID,treatment,week_num,air_temp) %>% 
  separate(fall_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  separate(spring_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>% 
  group_by(treatment,week_num) %>% 
  summarize(Air_Temp_std=sd(air_temp),Air_Temp_Mean=mean(air_temp),Air_Temp_n=length(air_temp))%>%
  mutate(Air_Temp_St_Error=Air_Temp_std/sqrt(Air_Temp_n)) %>% 
  ungroup()

# Graph that bad boy

All_TempGraph <- ggplot(TempSubset,aes(x=week_num, y=Air_Temp_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  xlab(element_blank())+
  ylab(element_blank())+
  expand_limits(y=c(10,30))+
  theme_bw() +
  theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) 

# View Graph

All_TempGraph

#
#Check for normality
shapiro.test(TempSubset$Air_Temp_Mean)
# p = .253 NORMAL :)

parametric_temp_aov <- aov(data = TempSubset, Air_Temp_Mean ~ treatment)
summary(temp_aov)
# P = .313 --- No signif :(

###
# Abiotic Humidity Figure
###

# Make a cute lil subset

HumiditySubset <- df_all %>% 
  select(fall_plant_ID,spring_plant_ID,treatment,week_num,humidity) %>% 
  separate(fall_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  separate(spring_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>% 
  group_by(treatment,week_num) %>% 
  summarize(humidity_std=sd(humidity),humidity_Mean=mean(humidity),humidity_n=length(humidity))%>%
  mutate(humidity_St_Error=humidity_std/sqrt(humidity_n)) %>% 
  ungroup()

# Graph that bad boy

All_HumidityGraph <- ggplot(HumiditySubset,aes(x=week_num, y=humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=humidity_Mean-humidity_St_Error,ymax=humidity_Mean+humidity_St_Error),width=0.2)+
  xlab(element_blank())+
  ylab(element_blank())+
  expand_limits(y=c(10,30))+
  theme_bw() +
  theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) 

# Look at it

All_HumidityGraph

#
#Check for normality
shapiro.test(HumiditySubset$humidity_Mean)
# p = .233 NORMAL :)

parametric_humidity_aov <- aov(data = HumiditySubset, humidity_Mean ~ treatment)
summary(parametric_humidity_aov)
# P = .106 --- No signif :(


###
# Abiotic Soil Moisture Figure
###

# Make a cute lil subset

SMSubset <- df_all %>% 
  select(fall_plant_ID,spring_plant_ID,treatment,week_num,soil_moisture) %>% 
  separate(fall_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  separate(spring_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>% 
  drop_na(soil_moisture) %>% 
  group_by(treatment,week_num) %>% 
  summarize(sm_std=sd(soil_moisture),sm_Mean=mean(soil_moisture),sm_n=length(soil_moisture))%>%
  mutate(sm_St_Error=sm_std/sqrt(sm_n)) %>% 
  ungroup() %>% 
  na.omit()

# Graph that bad boy

All_SMGraph <- ggplot(SMSubset,aes(x=week_num, y=sm_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=sm_Mean-sm_St_Error,ymax=sm_Mean+sm_St_Error),width=0.2)+
  xlab(element_blank())+
  ylab(element_blank())+
  expand_limits(y=c(10,30))+
  theme_bw() +
  theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) 

# Look at it

All_SMGraph

#Check for normality
shapiro.test(SMSubset$sm_Mean)
# p = .01675 NOT NORMAL :(

kruskal.test(data = SMSubset, sm_Mean ~ treatment)
# p = .140 --- NOT SIGNIF :(

###
# Abiotic Light Figure
###

# Make a cute lil subset

LightSubset <- df_all %>% 
  select(fall_plant_ID,spring_plant_ID,treatment,week_num,light_avail) %>% 
  separate(fall_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  separate(spring_plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>% 
  drop_na(light_avail) %>% 
  group_by(treatment, week_num) %>% 
  summarize(light_std=sd(light_avail),light_Mean=mean(light_avail),light_n=length(light_avail))%>%
  mutate(light_St_Error=light_std/sqrt(light_n)) %>% 
  ungroup()

# Graph that bad boy

All_LightGraph <- ggplot(LightSubset,aes(x=week_num, y=light_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=light_Mean-light_St_Error,ymax=light_Mean+light_St_Error),width=0.2)+
  xlab(element_blank())+
  ylab(element_blank())+
  expand_limits(y=c(10,30))+
  theme_bw() +
  theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) 

#Look at it

All_LightGraph

#Check for normality
shapiro.test(LightSubset$light_Mean)
# p = .010 --- NOT NORMAL :(

kruskal.test(data = LightSubset, light_Mean ~ treatment)
# p = .718 --- NOT SIGNIFICANT :(
