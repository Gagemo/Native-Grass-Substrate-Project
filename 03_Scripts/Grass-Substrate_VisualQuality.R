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

GRASS <- read.csv("02_Clean_Data/Grass_Substrate_Data_Height-Visual.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix-55BK", "A1", "B1", "ProMix-Bx"))

####################### Two-Way ANOVA Visual Data ##############################

two.way <- 
  aov(Visual.Quality ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="05_Figures/Two_Way_Visual.doc")
## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## VISUAL QUALITY WAS SIGNIFICANTLY AFFECTED BY SOIL  ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)

##################  One-Way ANOVA - Visual Quality #############################
    ### Create Individual Data Frames to Analyze Variance per Species ###

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Visual.Quality ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Visual.Quality ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Visual.Quality ~ Soil, data = sugar)
summary(s.one.way)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test<-TukeyHSD(two.way)
tukey.plot.test

HSD = HSD.test(two.way, trt = c("Species","Soil"))
HSD
capture.output(summary(HSD), file="05_Figures/Two_Way_Visual_HSD.doc")

## SIGNIFICANCE: SUGARCANE: BX VS A1 --- INDIAN: BX VS A1 ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################### Box Plot - Average Max Growth Heights ######################

VQ.S = 
  ggplot(GRASS, aes(x=Soil, y=Visual.Quality, fill = Soil)) + 
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
  ylab("Shoot Visual Quality (Poor - High)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))
VQ.S

ggsave("05_Figures/Shoot_Visual.png", VQ.S, bg='white',
       scale = 1, width = 12, height = 9, dpi = 500)
