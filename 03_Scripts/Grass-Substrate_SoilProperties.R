################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
#########################  Soil Properties Data ################################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

##########################    Installs Packages   ##############################

list.of.packages <- c("tidyverse", "agricolae", "labelled", 
                      "multcompView", "ggsignif", "car")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

library(tidyverse)
library(labelled)
library(agricolae)
library(multcompView)
library(ggsignif)
library(car)

########################### Load Data ##########################################

GRASS <- read.csv("02_Clean_Data/Grass Substrate Project - Soil Data.csv")

### Calculates Averages ###

Soil_Prop = data.frame(tapply(GRASS$Mass, GRASS$Soil, mean), 
                       tapply(GRASS$Water.Volume, GRASS$Soil, mean),
                       tapply(GRASS$Wet.Mass, GRASS$Soil, mean),
                       tapply(GRASS$Dry.Mass, GRASS$Soil, mean))
colnames(Soil_Prop) <- c("Mass_Ave","Water_Volume_Ave", "Wet_Ave", "Dry_Ave")

### Calculates Bulk Density, Particle Density, Air Filled Porosity,         ###
### Container Capacity, Total Porosity, Moisture Content                    ###
# 5.55 grams represents pot weight and 160 mL represents volume of container ##

GRASS$Bulk = ((GRASS$Mass)-(5.55))/160
GRASS$ParticleDensity = ((GRASS$Dry.Mass)/160)
GRASS$AirPorosity = ((GRASS$Water.Volume*100)/160)
GRASS$ContainerCapacity = ((((GRASS$Wet.Mass) - 5.55)*100)/160)
GRASS$TotalPorosity = (GRASS$AirPorosity+GRASS$ContainerCapacity)
GRASS$MoistureContent = ((GRASS$Mass - GRASS$Dry.Mass)*100)/5.55

####################### ANOVA ##############################

ANOVA_Bulk <- 
  aov(Bulk ~ Soil, data = GRASS)
summary(ANOVA_Bulk)
TukeyHSD(ANOVA_Bulk)
leveneTest(Bulk ~ Soil, data = GRASS)

ANOVA_ParticleDensity <- 
  aov(ParticleDensity ~ Soil, data = GRASS)
summary(ANOVA_ParticleDensity)
TukeyHSD(ANOVA_ParticleDensity)
leveneTest(ParticleDensity ~ Soil, data = GRASS)

ANOVA_AirPorosity <- 
  aov(AirPorosity ~ Soil, data = GRASS)
summary(ANOVA_AirPorosity)
TukeyHSD(ANOVA_AirPorosity)
leveneTest(AirPorosity ~ Soil, data = GRASS)

ANOVA_ContainerCapacity <- 
  aov(ContainerCapacity ~ Soil, data = GRASS)
summary(ANOVA_ContainerCapacity)
TukeyHSD(ANOVA_ContainerCapacity)
leveneTest(ContainerCapacity ~ Soil, data = GRASS)

ANOVA_TotalPorosity <- 
  aov(TotalPorosity ~ Soil, data = GRASS)
summary(ANOVA_TotalPorosity)
TukeyHSD(ANOVA_TotalPorosity)
leveneTest(TotalPorosity ~ Soil, data = GRASS)

ANOVA_MoistureContent <- 
  aov(MoistureContent ~ Soil, data = GRASS)
summary(ANOVA_MoistureContent)
TukeyHSD(ANOVA_MoistureContent)
leveneTest(MoistureContent ~ Soil, data = GRASS)




