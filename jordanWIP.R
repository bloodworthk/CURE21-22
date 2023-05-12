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
# Abiotic Temperature Figure
###




