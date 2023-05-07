#### Create Abiotic Through Time graphs for LLA Poster 2022 ####

#### Load in packages ####
library(tidyverse)
library(githubinstall)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
library(ggplot2)
library(grid)
#install.packages("scales")                  # Install scales package
library(scales) 
#install.packages("patchwork")
library(patchwork)

#### Set working directory ####
#KB - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/CURE_2021-2022/")

####### Read-In Datasets ######
Abiotics_Fall21<-read.csv("fall2021_llp315cure_499data.csv") %>% 
  mutate(semester="Fall_21")
  
Abiotics_Spring22<-read.csv("spring2022_llp315cure_499data.csv") %>% 
  mutate(semester="Spring_22") %>% 
  select(-X,-X.1,-X.2,-X.3,-X.4,-X.5,-X.6)

#### Merge and Clean Data ####
Abiotics_All<-Abiotics_Fall21 %>% 
  rbind(Abiotics_Spring22) %>% 
  select(semester,week_num,plant_ID,tray_ID,treatment,soil_moisture,light_avail,air_temp,humidity,comments) %>% 
  mutate(treatment=ifelse(treatment=="heat","heatwave",ifelse(treatment=="Control","control",ifelse(treatment=="Heatwave","heatwave",ifelse(treatment=="HeatWave","heatwave",treatment))))) %>% 
  mutate(air_temp=ifelse(air_temp==59,15,ifelse(air_temp==60,15.556,ifelse(air_temp==61,16.111,air_temp)))) %>% 
  filter(soil_moisture!="") %>% 
  filter(soil_moisture!="-")

#Create data frames for averages for graphs by abiotic factor

Humidity<-Abiotics_All %>% 
  select(semester,plant_ID,treatment,week_num,humidity) %>% 
  separate(plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  na.omit() %>% 
  group_by(semester,treatment,week_num) %>% 
  summarize(Humidity_std=sd(humidity),Humidity_Mean=mean(humidity),Humidity_n=length(humidity))%>%
  mutate(Humidity_St_Error=Humidity_std/sqrt(Humidity_n)) %>% 
  ungroup()

Air_Temp<-Abiotics_All %>% 
  select(semester,plant_ID,treatment,week_num,air_temp) %>% 
  separate(plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  na.omit() %>% 
  group_by(semester,treatment,week_num) %>% 
  summarize(Air_Temp_std=sd(air_temp),Air_Temp_Mean=mean(air_temp),Air_Temp_n=length(air_temp))%>%
  mutate(Air_Temp_St_Error=Air_Temp_std/sqrt(Air_Temp_n)) %>% 
  ungroup()

Soil_Moisture<-Abiotics_All %>% 
  select(semester,plant_ID,treatment,week_num,soil_moisture) %>% 
  separate(plant_ID,c("Day","Treatment","Plant_Number"),sep="-") %>%
  na.omit() %>% 
  group_by(semester,treatment,week_num) %>%
  summarize(soil_moisture_std=sd(as.numeric(soil_moisture)),soil_moisture_Mean=mean(as.numeric(soil_moisture)),soil_moisture_n=length(soil_moisture)) %>%
  mutate(soil_moisture_St_Error=soil_moisture_std/sqrt(soil_moisture_n)) %>% 
  ungroup()

####Graph #### 

#### Graph Humidity - spring 22
Sp_Humidity<-ggplot(subset(Humidity,semester=="Spring_22"),aes(x=week_num, y=Humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,90))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.title=element_blank(),legend.background=element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=19.5, y=100,label="Spring 2022",size=10)

#### Graph Humidity - fall 21
Fa_Humidity<-ggplot(subset(Humidity,semester=="Fall_21"),aes(x=as.factor(week_num), y=Humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Average Humidity (%)")+
  expand_limits(y=c(0,100))+
  expand_limits(x=c(1,9))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=2,y=100,label="Fall 2021",size=10)


#### Graph Air Temp - spring 22
Sp_Air_Temp<-ggplot(subset(Air_Temp,semester=="Spring_22"),aes(x=week_num, y=Air_Temp_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(10,30))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=19.5, y=30,label="Spring 2022",size=10)

#### Graph Air Temp - fall 21
Fa_Air_Temp<-ggplot(subset(Air_Temp,semester=="Fall_21"),aes(x=as.factor(week_num), y=Air_Temp_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab("Average Air Temp (C)")+
  expand_limits(y=c(10,30))+
  expand_limits(x=c(1,9))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=2.5, y=30,label="Fall 2021",size=10)

#### Graph Humidity - spring 22
Sp_Humidity<-ggplot(subset(Humidity,semester=="Spring_22"),aes(x=week_num, y=Humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,90))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.title=element_blank(),legend.background=element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=19.5, y=100,label="Spring 2022",size=10)

#### Graph Humidity - fall 21
Fa_Humidity<-ggplot(subset(Humidity,semester=="Fall_21"),aes(x=as.factor(week_num), y=Humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Average Humidity (%)")+
  expand_limits(y=c(0,100))+
  expand_limits(x=c(1,9))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=2,y=100,label="Fall 2021",size=10)

#### Graph Soil Moisture - spring 22
Sp_SM<-ggplot(subset(Soil_Moisture,semester=="Spring_22"),aes(x=week_num, y=soil_moisture_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=soil_moisture_Mean-soil_moisture_St_Error,ymax=soil_moisture_Mean+soil_moisture_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,15))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.title=element_blank(),legend.background=element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=19.5, y=15,label="Spring 2022",size=10)

#### Graph Humidity - fall 21
Fa_SM<-ggplot(subset(Soil_Moisture,semester=="Fall_21"),aes(x=as.factor(week_num), y=soil_moisture_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=5)+
  geom_line(aes(color=treatment,linetype=treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=soil_moisture_Mean-soil_moisture_St_Error,ymax=soil_moisture_Mean+soil_moisture_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Average Soil Moisture (C)")+
  expand_limits(y=c(0,15))+
  expand_limits(x=c(1,9))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  #geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  #geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw() +
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=2,y=15,label="Fall 2021",size=10)


pushViewport(viewport(layout=grid.layout(3,2)))
print(Fa_Air_Temp,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Sp_Air_Temp,vp=viewport(layout.pos.row=1, layout.pos.col =2))
print(Fa_Humidity,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(Sp_Humidity,vp=viewport(layout.pos.row=2, layout.pos.col =2))
print(Fa_SM,vp=viewport(layout.pos.row=3, layout.pos.col =1))
print(Sp_SM,vp=viewport(layout.pos.row=3, layout.pos.col =2))
#Save at 3000 x 2000  

#### Combining all Abiotics from spring and fall into one ###

#### Graph Air Temp
Air_Temp_Fig<-ggplot(Air_Temp,aes(x=as.factor(week_num), y=Air_Temp_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=7)+
  geom_line(aes(color=treatment,linetype=treatment),size=3)+
  scale_linetype_manual(values=c("solid","dashed"),labels=c('Control','Heatwave'))+
  scale_color_manual(values = c("#96A0A8","#755B3B"),labels=c('Control','Heatwave'))+
  scale_shape_manual(values=c(1,16), labels=c('Control','Heatwave'))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab("Temperature (C)")+
  expand_limits(y=c(10,40))+
  theme_bw() +
  theme(legend.position = c(0.89,0.88),legend.title=element_blank(),legend.background=element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=50),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"), legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'), legend.text = element_text(size=50)) +
  annotate("text",x=0.7,y=37,label="A.",size=20)

#### Graph Humidity
Humidity_Fig<-ggplot(Humidity,aes(x=as.factor(week_num), y=Humidity_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=7)+
  geom_line(aes(color=treatment,linetype=treatment),size=3)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = c("#96A0A8","#755B3B"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab("Humidity (%)")+
  expand_limits(y=c(0,100))+
  theme_bw() +
  theme(legend.position = "none", legend.title=element_blank(),legend.background=element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),text=element_text(size=50),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black")) +
  annotate("text",x=0.7,y=98,label="B.",size=20)

#Soil Moisture
SM_Fig<-ggplot(Soil_Moisture,aes(x=as.factor(week_num), y=soil_moisture_Mean,group=treatment))+
  geom_point(aes(color=treatment,shape=treatment),size=7)+
  geom_line(aes(color=treatment,linetype=treatment),size=3)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = c("#96A0A8","#755B3B"))+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=soil_moisture_Mean-soil_moisture_St_Error,ymax=soil_moisture_Mean+soil_moisture_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Soil Moisture (%)")+
  expand_limits(y=c(0,15))+
  theme_bw() +
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=50),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))   +
  annotate("text",x=0.7,y=14,label="C.",size=20)

Air_Temp_Fig+
  Humidity_Fig+
  SM_Fig+
  plot_layout(ncol = 1,nrow = 3)
#save at 2000x1800

pushViewport(viewport(layout=grid.layout(3,1)))
print(Air_Temp_Fig,vp=viewport(layout.pos.row=1, layout.pos.col =1))
print(Humidity_Fig,vp=viewport(layout.pos.row=2, layout.pos.col =1))
print(SM_Fig,vp=viewport(layout.pos.row=3, layout.pos.col =1))
#Save at 3000 x 2000 


#### T Tests


#Week 1
TTests_Week1<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==1) %>% 
  ungroup()

wilcox.test(TTests_Week1$soil_moisture~TTests_Week1$treatment) #p=0.06
wilcox.test(TTests_Week1$air_temp~TTests_Week1$treatment) #p=0.0001116 
wilcox.test(TTests_Week1$humidity~TTests_Week1$treatment) #p=1

#Week 2
TTests_Week2<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==2) %>% 
  ungroup()

wilcox.test(TTests_Week2$soil_moisture~TTests_Week2$treatment) #p=0.207
wilcox.test(TTests_Week2$air_temp~TTests_Week2$treatment) #NA
wilcox.test(TTests_Week2$humidity~TTests_Week2$treatment) #p=1

#Week 3
TTests_Week3<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==3) %>% 
  ungroup()

wilcox.test(TTests_Week3$soil_moisture~TTests_Week3$treatment) #p=<2.2e-16
wilcox.test(TTests_Week3$air_temp~TTests_Week3$treatment) #p<2.2e-16
wilcox.test(TTests_Week3$humidity~TTests_Week3$treatment) #p=1.147e-11

#Week 4
TTests_Week4<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==4) %>% 
  ungroup()

wilcox.test(TTests_Week4$soil_moisture~TTests_Week4$treatment) #p=<2.2e-16
wilcox.test(TTests_Week4$air_temp~TTests_Week4$treatment) #p<2.2e-16
wilcox.test(TTests_Week4$humidity~TTests_Week4$treatment) #p<2.2e-16

#Week 5
TTests_Week5<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==5) %>% 
  ungroup()

wilcox.test(TTests_Week5$soil_moisture~TTests_Week5$treatment) #p=9.339e-05
wilcox.test(TTests_Week5$air_temp~TTests_Week5$treatment) #p=0.5344
wilcox.test(TTests_Week5$humidity~TTests_Week5$treatment) #p=0.3662

#Week 9
TTests_Week9<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==9) %>% 
  ungroup()

wilcox.test(TTests_Week9$soil_moisture~TTests_Week5$treatment) #p=0.003279
wilcox.test(TTests_Week9$air_temp~TTests_Week5$treatment) #p=0.0006724
wilcox.test(TTests_Week9$humidity~TTests_Week5$treatment) #p=7.4e-05

#Week 18
TTests_Week18<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==18) %>% 
  ungroup()

wilcox.test(TTests_Week18$soil_moisture~TTests_Week18$treatment) #p=0.114
wilcox.test(TTests_Week18$air_temp~TTests_Week18$treatment) #p=1.871e-12
wilcox.test(TTests_Week18$humidity~TTests_Week18$treatment) #p=0.1898

#Week 19
TTests_Week19<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==19) %>% 
  ungroup()

wilcox.test(TTests_Week19$soil_moisture~TTests_Week19$treatment) #p=0.02228
wilcox.test(TTests_Week19$air_temp~TTests_Week19$treatment) #p=2.055e-11
wilcox.test(TTests_Week19$humidity~TTests_Week19$treatment) #p< 2.2e-16

#Week 20
TTests_Week20<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==20) %>% 
  ungroup()

wilcox.test(TTests_Week20$soil_moisture~TTests_Week20$treatment) #p=2.088e-05
wilcox.test(TTests_Week20$air_temp~TTests_Week20$treatment) #p< 2.2e-16
wilcox.test(TTests_Week20$humidity~TTests_Week20$treatment) #p< 2.2e-16

#Week 21
TTests_Week21<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==21) %>% 
  ungroup()

wilcox.test(TTests_Week21$soil_moisture~TTests_Week21$treatment) #p=1.053e-13
wilcox.test(TTests_Week21$air_temp~TTests_Week21$treatment) #p=0.0009162
wilcox.test(TTests_Week21$humidity~TTests_Week21$treatment) #p=0.01589

#Week 22
TTests_Week22<-Abiotics_All%>%
  mutate(soil_moisture=as.numeric(soil_moisture)) %>% 
  select(week_num,soil_moisture,air_temp,humidity,treatment) %>% 
  group_by(week_num,soil_moisture,air_temp,humidity,treatment)%>%
  filter(week_num==22) %>% 
  ungroup()

wilcox.test(TTests_Week22$soil_moisture~TTests_Week22$treatment) #p=0.0003918
wilcox.test(TTests_Week22$air_temp~TTests_Week22$treatment) #p=7.773e-14 - with warning
wilcox.test(TTests_Week22$humidity~TTests_Week22$treatment) #NA - with warning
