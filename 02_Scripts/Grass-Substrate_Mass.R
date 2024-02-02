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

################## Two-Way ANOVA Shoot Mass ###############################

two.way.S <- 
  aov(Shoot_Weight ~ Species*Soil, data = GRASS)
summary(two.way.S)
capture.output(summary(two.way.S), file="03_Figures/Two_Way_Shoot.doc")

## INTERACTION BETWEEN SPECIES & SOIL SIGNIFICANT ##
## SHOOT MASS WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way.S)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test.S<-TukeyHSD(two.way.S)
tukey.plot.test.S

HSD.S = HSD.test(two.way.S, trt = c("Species","Soil"))
HSD.S

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

supp.labs <- c("Indiangrass","Sugarcane Plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

SHOOT = 
  ggplot(GRASS, aes(x=Soil, y=Shoot_Weight, fill = Soil)) + 
  geom_violin(show.legend = FALSE) +
  geom_text(data = dt, aes(label = tukey.cld, y = 18), 
            vjust = -0.5, size=10) +
  ylim(0,20) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 30, face="bold"),
        axis.text.y = element_text(size=30, face="bold", color = "black"),
        panel.background = element_rect(color=NA),
        plot.background = element_rect(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(color=NA),
        legend.box.background = element_rect(color=NA),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", size = 30, face="bold"),
        legend.text = element_text(color = "black", size = 30),
        strip.text = element_text(color = "black", size = 30, face="bold"),
        text = element_text(family = "sans"),
        axis.ticks.x=element_blank())  +
  ylab("Dry shoot mass (g)") +
  xlab("") +
  scale_fill_manual(name = "Substrate media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
SHOOT

ggsave("03_Figures/Shoot.Mass.png", SHOOT, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)

################## Two-Way ANOVA Root/Shoot Mass ###############################
two.way.R <- 
  aov(Root_Weight ~ Species*Soil, data = GRASS)
summary(two.way.R)
capture.output(summary(two.way.R), file="03_Figures/Two_Way_Root.doc")

## INTERACTION BETWEEN SPECIES & SOIL SIGNIFICANT ##
## ROOT MASS WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way.R)

########################## Tukey Test - Multiple Comparisons ###################

tukey.plot.test.R<-TukeyHSD(two.way.R)
tukey.plot.test.R

HSD.R = HSD.test(two.way.R, trt = c("Species","Soil"))
HSD.R

##################      Creates Storage for       ##############################
################## Significance Letters for Graph ##############################

tukey.cld <- multcompLetters4(two.way.R, tukey.plot.test.R)
print(tukey.cld)

dt <- GRASS %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Root_Weight)), 
            sd = sd(exp(Root_Weight)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,
                       levels = c("ProMix55BK", "NativeMix", 
                                  "GardenMix", "ProMixBx"),
                       ordered = TRUE))

cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Root/Shoot Mass #################################

supp.labs <- c("Indiangrass","Sugarcane Plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

ROOT = 
  ggplot(GRASS, aes(x=Soil, y=Root_Weight, fill = Soil)) + 
  geom_violin() +
  geom_text(data = dt, aes(label = tukey.cld, y = 35), 
            vjust = -0.5, size=10) +
  ylim(0,40) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 30, face="bold"),
        axis.text.y = element_text(size=30, face="bold", color = "black"),
        panel.background = element_rect(color=NA),
        plot.background = element_rect(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(color=NA),
        legend.box.background = element_rect(color=NA),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", size = 30, face="bold"),
        legend.text = element_text(color = "black", size = 30),
        legend.position="bottom",
        strip.text = element_text(color = "black", size = 30, face="bold"),
        text = element_text(family = "sans"),
        axis.ticks.x=element_blank())  +
  ylab("Dry root mass (g)") +
  xlab("") +
  scale_fill_manual(name = "Substrate media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
ROOT

ggsave("03_Figures/Root.Mass.png", ROOT, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)

# Combine both graphs #
combined = ggarrange(SHOOT, ROOT, ncol = 1, nrow = 2)
ggsave("03_Figures/Combined_Mass.png", combined, bg='white',
       scale = 1, width = 18, height = 12, dpi = 1500)

#################### Correlation Test - Shoot/Root Mass ########################

cor.test(GRASS$Shoot_Weight, GRASS$Root_Weight)

