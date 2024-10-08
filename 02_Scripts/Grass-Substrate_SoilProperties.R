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
                      "multcompView", "ggsignif", "car", "tables")
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
library(tables)

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Soil Data.csv")

### Calculates Averages ###

Soil_Prop = data.frame(tapply(GRASS$Mass, GRASS$Soil, mean), 
                       tapply(GRASS$Water_Volume, GRASS$Soil, mean),
                       tapply(GRASS$Wet_Mass, GRASS$Soil, mean),
                       tapply(GRASS$Dry_Mass, GRASS$Soil, mean))
colnames(Soil_Prop) <- c("Mass_Ave","Water_Volume_Ave", "Wet_Ave", "Dry_Ave")

### Calculates Bulk Density, Particle Density, Air Filled Porosity,         ###
### Container Capacity, Total Porosity, Moisture Content                    ###
# 5.55 grams represents pot weight and 160 mL represents volume of container ##

GRASS$Bulk = ((GRASS$Mass)-(5.55))/160
GRASS$ParticleDensity = ((GRASS$Dry_Mass)/160)
GRASS$AirPorosity = ((GRASS$Water_Volume*100)/160)
GRASS$ContainerCapacity = ((((GRASS$Wet_Mass) - 5.55)*100)/160)
GRASS$TotalPorosity = (GRASS$AirPorosity+GRASS$ContainerCapacity)
GRASS$MoistureContent = ((GRASS$Mass - GRASS$Dry_Mass)*100)/5.55

MEAN = GRASS %>%
  group_by(Soil) %>%
  summarise_at(vars(Mass, Water_Volume, Wet_Mass, Dry_Mass, Bulk, 
                    ParticleDensity, AirPorosity, ContainerCapacity, 
                    TotalPorosity, MoistureContent), list(name = mean))
MEAN = MEAN %>% select(c(Soil,MoistureContent_name,AirPorosity_name,
                           TotalPorosity_name,ContainerCapacity_name,
                           Bulk_name,ParticleDensity_name))  

####################### ANOVA ##############################


ANOVA_MoistureContent <- 
  aov(MoistureContent ~ Soil, data = GRASS)
summary(ANOVA_MoistureContent)
HSD = HSD.test(ANOVA_MoistureContent, trt = c("Soil"))
HSD
leveneTest(MoistureContent ~ Soil, data = GRASS)

ANOVA_AirPorosity <- 
  aov(AirPorosity ~ Soil, data = GRASS)
summary(ANOVA_AirPorosity)
HSD = HSD.test(ANOVA_AirPorosity, trt = c("Soil"))
HSD
leveneTest(AirPorosity ~ Soil, data = GRASS)

ANOVA_TotalPorosity <- 
  aov(TotalPorosity ~ Soil, data = GRASS)
summary(ANOVA_TotalPorosity)
HSD = HSD.test(ANOVA_TotalPorosity, trt = c("Soil"))
HSD
leveneTest(TotalPorosity ~ Soil, data = GRASS)

ANOVA_ContainerCapacity <- 
  aov(ContainerCapacity ~ Soil, data = GRASS)
summary(ANOVA_ContainerCapacity)
HSD = HSD.test(ANOVA_ContainerCapacity, trt = c("Soil"))
HSD
leveneTest(ContainerCapacity ~ Soil, data = GRASS)

ANOVA_Bulk <- 
  aov(Bulk ~ Soil, data = GRASS)
summary(ANOVA_Bulk)
HSD = HSD.test(ANOVA_Bulk, trt = c("Soil"))
HSD
leveneTest(Bulk ~ Soil, data = GRASS)

ANOVA_ParticleDensity <- 
  aov(ParticleDensity ~ Soil, data = GRASS)
summary(ANOVA_ParticleDensity)
HSD = HSD.test(ANOVA_ParticleDensity, trt = c("Soil"))
HSD
leveneTest(ParticleDensity ~ Soil, data = GRASS)




