################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
#########################  Height Data Analysis ################################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

##########################    Installs Packages   ##############################

list.of.packages <- c("tidyverse", "agricolae", "labelled", 
                      "multcompView", "ggsignif", "showtext")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

library(tidyverse)
library(labelled)
library(agricolae)
library(multcompView)
library(ggsignif)
library(showtext)

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv", 
                  check.names=FALSE)

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

################## Two-Way ANOVA Average Max Growth Heights ####################

two.way <- 
  aov(Average_Max_Height ~ Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_AvgMaxHgt.doc")

## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)

########################## Tukey Test - Multiple Comparisons ###################

#tukey <-TukeyHSD(two.way)
#tukey

#HSD = HSD.test(two.way, trt = c("Species","Soil"))
#HSD

##################  One-Way ANOVA - Average Max Growth Heights #################
### Create Individual Data Frames to Analyze Variance per Species ##############

indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Average_Max_Height ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Average_Max_Height ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Average_Max_Height ~ Soil, data = sugar)
summary(s.one.way)

i.tukey <- TukeyHSD(i.one.way)
i.tukey
w.tukey <- TukeyHSD(w.one.way)
w.tukey
s.tukey <- TukeyHSD(s.one.way)
s.tukey

## SIGNIFICANCE: SUGARCANE: BX VS A1 --- INDIAN: BX VS A1 ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

#tukey.cld <- multcompLetters4(two.way, tukey)
#print(tukey.cld)

#dt <- GRASS %>% 
#  group_by(Species, Soil) %>%
#  summarise(w=mean(exp(Average_Max_Height)), 
#            sd = sd(exp(Average_Max_Height)) / sqrt(n())) %>%
#  arrange(desc(w)) %>% 
#  ungroup() %>% 
#  mutate(Soil = factor(Soil,
#                            levels = c("ProMix55BK", "NativeMix", 
#                                       "GardenMix", "ProMixBx"),
#                            ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
#cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
#dt$tukey.cld <- cld2$letters

################### Box Plot - Average Max Growth Heights ######################

Avg_Max = 
ggplot(GRASS, aes(x = Soil, y = Average_Max_Height, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE) +
  facet_grid(. ~ Species) +
  geom_signif(comparisons = list(c("NativeMix", "ProMixBx")), 
              size=0.8, textsize=5, fontface = "bold") +
  #geom_text(data = dt, aes(label = tukey.cld, y = 95), vjust = -0.5) +
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
        axis.ticks.x=element_blank()) +
  ylab("Average Max Height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("ProMix 55BK", "Native Mix", 
                               "Garden Mix", "ProMix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
Avg_Max

ggsave("03_Figures/Average.Max.Growth.Height2.png", Avg_Max, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)

