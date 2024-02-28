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

list.of.packages <- c("tidyverse", "agricolae", "labelled", "ggpubr",
                      "multcompView", "ggsignif", "showtext", "extrafont",
                      "rstatix", "likert")
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
library(ggpubr)
library(extrafont)
library(rstatix)
library(likert)

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

####################### Two-Way ANOVA Visual Shoot Data ########################

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
  aov(Root_Quality_Scale ~ Species*Soil, data = GRASS) %>% add_significance()
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_Root_Visual.doc")

tukey <-TukeyHSD(two.way) 
tukey

HSD = HSD.test(two.way, trt = c("Species","Soil"))
HSD

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

################################################################################
# Extract the residuals
aov_residuals <- residuals(object = two.way)

tukey.cld <- multcompLetters4(two.way, tukey)
print(tukey.cld)

# Makes Letters for graph below #
dt <- GRASS %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Root_Quality_Scale)), 
            sd = sd(exp(Root_Quality_Scale)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", 
                                       "GardenMix", "ProMixBx"),ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Breaking Point Height ######################

supp.labs <- c("Indiangrass","Sugarcane Plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

ROOT_VISUAL = 
  ggplot(GRASS, aes(x=Soil, y=Root_Quality_Scale, fill = Soil)) + 
  geom_violin(size = 0.5, color="black") +
  geom_point(shape=16, show.legend = FALSE, size = 2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = dt, aes(label = tukey.cld, y = 4.25), size=10, vjust = -0.5) +
  ylim(0, 4.5) +
  theme_bw() +
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
        axis.ticks.x=element_blank()) +
  ylab("Visual scale") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
ROOT_VISUAL

ggsave("03_Figures/Root.Visual.png", ROOT_VISUAL, bg='white',
       scale = 1, width = 16, height = 6, dpi = 1500)

