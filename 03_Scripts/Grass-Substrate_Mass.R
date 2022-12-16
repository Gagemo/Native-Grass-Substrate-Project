################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
###################### Root & Shoot Mass Data Analysis #########################
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

GRASS <- read.csv("02_Clean_Data/Grass_Substrate_Data_Mass.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix-55BK", "A1", "B1", "ProMix-Bx"))

################## Two-Way ANOVA Root/Shoot Mass ###############################

two.way.S <- 
  aov(Shoot.Weight ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way.S)
capture.output(summary(two.way.S), file="05_Figures/Two_Way_Shoot.doc")
## INTERACTION BETWEEN SPECIES & SOIL SIGNIFICANT ##
## SHOOT MASS WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

two.way.R <- 
  aov(Root.Weight ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way.R)
capture.output(summary(two.way.R), file="05_Figures/Two_Way_Root.doc")
## INTERACTION BETWEEN SPECIES & SOIL SIGNIFICANT ##
## ROOT MASS WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way.S)

par(mfrow=c(2,2))
plot(two.way.R)

##################  One-Way ANOVA - Shoot/Root Mass #################
    ### Create Individual Data Frames to Analyze Variance per Species ###

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way.S <- aov(Shoot.Weight ~ Soil, data = indian)
summary(i.one.way.S)

w.one.way.S <- aov(Shoot.Weight ~ Soil, data = wire)
summary(w.one.way.S)

s.one.way.S <- aov(Shoot.Weight ~ Soil, data = sugar)
summary(s.one.way.S)

i.one.way.R <- aov(Root.Weight ~ Soil, data = indian)
summary(i.one.way.R)

w.one.way.R <- aov(Root.Weight ~ Soil, data = wire)
summary(w.one.way.R)

s.one.way.R <- aov(Root.Weight ~ Soil, data = sugar)
summary(s.one.way.R)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test.S<-TukeyHSD(two.way.S)
tukey.plot.test.S

tukey.plot.test.R<-TukeyHSD(two.way.R)
tukey.plot.test.R

HSD = HSD.test(two.way.S, trt = c("Species","Soil"))
HSD

HSD = HSD.test(two.way.R, trt = c("Species","Soil"))
HSD

i.tukey.S <- TukeyHSD(i.one.way.S)
i.tukey.S
w.tukey.S<-TukeyHSD(w.one.way.S)
w.tukey.S
s.tukey.S<-TukeyHSD(s.one.way.S)
s.tukey.S

i.tukey.R <- TukeyHSD(i.one.way.R)
i.tukey.R
w.tukey.R<-TukeyHSD(w.one.way.R)
w.tukey.R
s.tukey.R<-TukeyHSD(s.one.way.R)
s.tukey.R

#################### Correlation Test - Shoot/Root Mass ########################

cor.test(GRASS$Shoot.Weight, GRASS$Root.Weight)

################### Box Plot - Root/Shoot Mass #################################

SHOOT = 
  ggplot(GRASS, aes(x=Soil, y=Shoot.Weight, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  geom_signif(comparisons = list(c("A1", "ProMix-Bx")), 
              map_signif_level = TRUE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 12, face="bold"),
        axis.text.x = element_text(face="bold"),
        panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        strip.background = element_blank()) +
  ylab("Dry Shoot Mass (g)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))
SHOOT

ggsave("05_Figures/Shoot.Mass.png", SHOOT, bg='white',
       scale = 1, width = 12, height = 9, dpi = 500)

ROOT = 
  ggplot(GRASS, aes(x=Soil, y=Root.Weight, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  geom_signif(comparisons = list(c("A1", "ProMix-Bx")), 
              map_signif_level = TRUE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 12, face="bold"),
        axis.text.x = element_text(face="bold"),
        panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        strip.background = element_blank()) +
  ylab("Dry Root Mass (g)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))
ROOT

ggsave("05_Figures/Root.Mass.png", ROOT, bg='white',
       scale = 1, width = 12, height = 9, dpi = 500)
