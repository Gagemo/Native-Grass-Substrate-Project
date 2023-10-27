################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
####################### Breaking Point Heights Analysis ########################
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

################## Two-Way ANOVA Breaking Point Height #########################

two.way <- 
  aov(Breaking_Point ~ Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_AvgMaxHgt.doc")
## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## BREAKING POINT HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)

##################  One-Way ANOVA - Breaking Point Height #################
### Create Individual Data Frames to Analyze Variance per Species ###

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Breaking_Point ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Breaking_Point ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Breaking_Point ~ Soil, data = sugar)
summary(s.one.way)

########################## Tukey Test - Multiple Comparisons ###################

i.tukey <- TukeyHSD(i.one.way)
i.tukey
w.tukey<-TukeyHSD(w.one.way)
w.tukey
s.tukey<-TukeyHSD(s.one.way)
s.tukey

## SIGNIFICANCE: SUGARCANE: BX VS A1 ##

################### Box Plot - Breaking Point Height ######################

BREAK = 
  ggplot(GRASS, aes(x=Soil, y=Breaking_Point, fill = Soil)) + 
  geom_violin(size = 0.5, color="black") +
  geom_point(shape=16, show.legend = FALSE) +
  geom_signif(comparisons = list(c("NativeMix", "ProMixBx")), 
              size=0.8, textsize=5, fontface = "bold") +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 20, face="bold"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        panel.background = element_rect(color=NA),
        plot.background = element_rect(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(color=NA),
        legend.box.background = element_rect(color=NA),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", size = 20, face="bold"),
        legend.text = element_text(color = "black", size = 20),
        legend.position="bottom",
        strip.text = element_text(color = "black", size = 20, face="bold"),
        text = element_text(family = "sans"),
        axis.ticks.x=element_blank()) +
  ylab("Breaking Point Height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("ProMix 55BK", "Native Mix", 
                               "Garden Mix", "ProMix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
BREAK

ggsave("03_Figures/Breaking.Height.png", BREAK, bg='white',
       scale = 1, width = 16, height = 6, dpi = 1500)


###### Correlation of Average Max Growth Height & Breaking Point Height ########

cor.test(GRASS$Average_Max_Height, GRASS$Breaking_Point)
