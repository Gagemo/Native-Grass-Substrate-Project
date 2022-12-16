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

GRASS <- read.csv("02_Clean_Data/Grass_Substrate_Data_Height-Visual.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix-55BK", "A1", "B1", "ProMix-Bx"))

################## Two-Way ANOVA Breaking Point Height #########################

two.way <- 
  aov(Breaking.Point ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="05_Figures/Two_Way_AvgMaxHgt.doc")
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

i.one.way <- aov(Breaking.Point ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Breaking.Point ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Breaking.Point ~ Soil, data = sugar)
summary(s.one.way)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test<-TukeyHSD(two.way)
tukey.plot.test

HSD = HSD.test(two.way, trt = c("Species","Soil"))
HSD

i.tukey <- TukeyHSD(i.one.way)
i.tukey
w.tukey<-TukeyHSD(w.one.way)
w.tukey
s.tukey<-TukeyHSD(s.one.way)
s.tukey

## SIGNIFICANCE: SUGARCANE: BX VS A1 ##

################### Box Plot - Breaking Point Height ######################

BREAK = 
  ggplot(GRASS, aes(x=Soil, y=Breaking.Point, fill = Soil)) + 
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
  ylab("Breaking Point Height (cm)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))
BREAK

ggsave("05_Figures/Breaking.Height.png", Avg_Max, bg='white',
       scale = 1, width = 12, height = 9, dpi = 500)


###### Correlation of Average Max Growth Height & Breaking Point Height ########

cor.test(GRASS$Average.Max.Height, GRASS$Breaking.Point)
