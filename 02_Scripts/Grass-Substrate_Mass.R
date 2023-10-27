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

GRASS <- read.csv("01_Data/Grass Substrate Project - Root_Shoot Mass.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

################## Two-Way ANOVA Root/Shoot Mass ###############################

two.way.S <- 
  aov(Shoot_Weight ~ Species*Soil, data = GRASS)
summary(two.way.S)
capture.output(summary(two.way.S), file="03_Figures/Two_Way_Shoot.doc")

## INTERACTION BETWEEN SPECIES & SOIL SIGNIFICANT ##
## SHOOT MASS WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

two.way.R <- 
  aov(Root_Weight ~ Species*Soil, data = GRASS)
summary(two.way.R)
capture.output(summary(two.way.R), file="03_Figures/Two_Way_Root.doc")

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

i.one.way.S <- aov(Shoot_Weight ~ Soil, data = indian)
summary(i.one.way.S)

w.one.way.S <- aov(Shoot_Weight ~ Soil, data = wire)
summary(w.one.way.S)

s.one.way.S <- aov(Shoot_Weight ~ Soil, data = sugar)
summary(s.one.way.S)

i.one.way.R <- aov(Root_Weight ~ Soil, data = indian)
summary(i.one.way.R)

w.one.way.R <- aov(Root_Weight ~ Soil, data = wire)
summary(w.one.way.R)

s.one.way.R <- aov(Root_Weight ~ Soil, data = sugar)
summary(s.one.way.R)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test.S<-TukeyHSD(two.way.S)
tukey.plot.test.S

tukey.plot.test.R<-TukeyHSD(two.way.R)
tukey.plot.test.R

HSD.S = HSD.test(two.way.S, trt = c("Species","Soil"))
HSD.S

HSD.R = HSD.test(two.way.R, trt = c("Species","Soil"))
HSD.R

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

cor.test(GRASS$Shoot_Weight, GRASS$Root_Weight)

##################      Creates Storage for       ##############################
################## Significance Letters for Graph ##############################

tukey.cld <- multcompLetters4(two.way.S, tukey.plot.test.S)
print(tukey.cld)

dt <- GRASS %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Shoot_Weight)), 
            sd = sd(exp(Shoot_Weight)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,
                            levels = c("ProMix55BK", "NativeMix", 
                                       "GardenMix", "ProMixBx"),
                            ordered = TRUE))

cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Root/Shoot Mass #################################

SHOOT = 
  ggplot(GRASS, aes(x=Soil, y=Shoot_Weight, fill = Soil)) + 
  geom_violin(show.legend = FALSE) +
  geom_text(data = dt, aes(label = tukey.cld, y = 18), 
            vjust = -0.5, size=6) +
 #geom_signif(comparisons = list(c("NativeMix", "ProMixBx"), 
 #                              c("ProMix55BK", "ProMixBx" )),
 #                                y_position = c(18, 20), 
 #             map_signif_level = TRUE) +
  facet_grid(. ~ Species) +
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
        strip.text = element_text(color = "black", size = 20, face="bold"),
        text = element_text(family = "sans"),
        axis.ticks.x=element_blank())  +
  ylab("Dry Shoot Mass (g)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("ProMix 55BK", "Native Mix", 
                               "Garden Mix", "ProMix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
SHOOT

ggsave("03_Figures/Shoot.Mass.png", SHOOT, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)

ROOT = 
  ggplot(GRASS, aes(x=Soil, y=Root_Weight, fill = Soil)) + 
  geom_violin() +
  geom_text(data = dt, aes(label = tukey.cld, y = 35), 
            vjust = -0.5, size=6) +
  #geom_signif(comparisons = list(c("NativeMix", "ProMixBx"), 
  #                              c("ProMix55BK", "ProMixBx" )),
  #                                y_position = c(18, 20), 
  #             map_signif_level = TRUE) +
  facet_grid(. ~ Species) +
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
        axis.ticks.x=element_blank())  +
  ylab("Dry Root Mass (g)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("ProMix 55BK", "Native Mix", 
                               "Garden Mix", "ProMix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
ROOT

ggsave("03_Figures/Root.Mass.png", ROOT, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)
