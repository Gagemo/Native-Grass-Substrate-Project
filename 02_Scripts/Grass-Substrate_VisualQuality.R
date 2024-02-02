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

HSD.test(two.way,
         trt = c("Species", "Soil"),
         console = TRUE # print results
)
TukeyHSD(two.way, which = "Species:Soil")

################# Shapiro-Wilk Test ############################################

# Extract the residuals
aov_residuals <- residuals(object = two.way)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)
vq <- GRASS$Visual_Quality
hist(vq)

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

HSD.test(two.way,
         trt = c("Species", "Soil"),
         console = TRUE # print results
)
TukeyHSD(two.way, which = "Species:Soil")

## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## VISUAL ROOT QUALITY WAS SIGNIFICANTLY AFFECTED BY SOIL  ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)
Rq <- GRASS$Root_Quality_Scale
hist(Rq)

################# Shapiro-Wilk Test ############################################

# Extract the residuals
aov_residuals <- residuals(object = two.way)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

