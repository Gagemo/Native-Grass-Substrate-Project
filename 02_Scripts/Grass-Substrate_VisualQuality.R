################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
#########################  Visual Data Analysis ################################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

##########################    Installs Packages   ##############################

list.of.packages <- c("tidyverse", "agricolae", "labelled", 
                      "multcompView", "ggsignif")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

library(tidyverse)
library(labelled)
library(agricolae)
library(multcompView)
library(ggsignif)

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

####################### Two-Way ANOVA Visual Data ##############################

two.way <- 
  aov(Visual_Quality ~ Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_Visual.doc")

## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## VISUAL QUALITY WAS SIGNIFICANTLY AFFECTED BY SOIL  ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)
vq <- GRASS$Visual_Quality
hist(vq)

##################  One-Way ANOVA - Visual Quality #############################
### Create Individual Data Frames to Analyze Variance per Species ###

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Visual_Quality ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Visual_Quality ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Visual_Quality ~ Soil, data = sugar)
summary(s.one.way)

i.tukey <- TukeyHSD(i.one.way)
i.tukey
w.tukey <- TukeyHSD(w.one.way)
w.tukey
s.tukey <- TukeyHSD(s.one.way)
s.tukey

################################################################################
################################################################################
#########################   Root Visual Data  ##################################
################################################################################
################################################################################

####################### Two-Way ANOVA Visual Data ##############################

two.way <- 
  aov(Root_Quality_Scale ~ Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_Root_Visual.doc")

## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## VISUAL ROOT QUALITY WAS SIGNIFICANTLY AFFECTED BY SOIL  ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)
Rq <- GRASS$Root_Quality_Scale
hist(Rq)

##################  One-Way ANOVA - Visual Quality #############################
### Create Individual Data Frames to Analyze Variance per Species ###

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Root_Quality_Scale ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Root_Quality_Scale ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Root_Quality_Scale ~ Soil, data = sugar)
summary(s.one.way)

i.tukey <- TukeyHSD(i.one.way)
i.tukey
w.tukey <- TukeyHSD(w.one.way)
w.tukey
s.tukey <- TukeyHSD(s.one.way)
s.tukey
