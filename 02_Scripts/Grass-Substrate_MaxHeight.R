################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
################### Height Data & Breaking Point Analysis ######################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

##########################    Installs Packages   ##############################

list.of.packages <- c("tidyverse", "agricolae", "labelled", "ggpubr",
                      "multcompView", "ggsignif", "showtext", "extrafont",
                      "rstatix")
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

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv", 
                  check.names=FALSE)

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

################## Two-Way ANOVA Average Max Growth Heights ####################

two.way <- 
  aov(Average_Max_Height ~ Species*Soil, data = GRASS) %>% add_significance()
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_MaxHgt.doc")

## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)

################# Shapiro-Wilk Test ############################################

# Extract the residuals
aov_residuals <- residuals(object = two.way)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

########################## Tukey Test - Multiple Comparisons ###################

tukey <-TukeyHSD(two.way) 
tukey

HSD = HSD.test(two.way, trt = c("Species","Soil"))
HSD

## SIGNIFICANCE: SUGARCANE: BX VS A1 --- INDIAN: BX VS A1 ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

tukey.cld <- multcompLetters4(two.way, tukey)
print(tukey.cld)

dt <- GRASS %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Average_Max_Height)), 
            sd = sd(exp(Average_Max_Height)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,
                            levels = c("ProMix55BK", "NativeMix", 
                                       "GardenMix", "ProMixBx"),
                            ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Max Growth Heights ######################
supp.labs <- c("Indiangrass","Sugarcane Plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

Avg_Max = 
ggplot(GRASS, aes(x = Soil, y = Average_Max_Height, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = dt, aes(label = tukey.cld, y = 95), size=10, vjust = -0.5) +
  ylim(0,105)+
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
        axis.ticks.x=element_blank()) +
  ylab("Max height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
Avg_Max

ggsave("03_Figures/Average.Max.Growth.Height2.png", Avg_Max, bg= NA,
       scale = 1, width = 16, height = 6, dpi = 1500)

################################################################################
################## Breaking Point Height #######################################
################################################################################

################## Two-Way ANOVA Breaking Point Height #########################

two.way <- 
  aov(Breaking_Point ~ Species*Soil, data = GRASS)
summary(two.way)
capture.output(summary(two.way), file="03_Figures/Two_Way_BreakingPoint.doc")
## INTERACTION BETWEEN SPECIES & SOIL NOT SIGNIFICANT ##
## BREAKING POINT HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

########################## Tukey Test - Multiple Comparisons ###################

tukey <-TukeyHSD(two.way) 
tukey

HSD = HSD.test(two.way, trt = c("Species","Soil"))
HSD
## SIGNIFICANCE: SUGARCANE: BX VS A1 ##

################## Plot Residuals, Q-Q Plot ####################################

par(mfrow=c(2,2))
plot(two.way)

################# Shapiro-Wilk Test ############################################

# Extract the residuals
aov_residuals <- residuals(object = two.way)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

tukey.cld <- multcompLetters4(two.way, tukey)
print(tukey.cld)

# Makes Letters for graph below #
dt <- GRASS %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Breaking_Point)), 
            sd = sd(exp(Breaking_Point)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", 
                                  "GardenMix", "ProMixBx"),ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species:Soil`$Letters)
dt$tukey.cld <- cld2$letters
################### Box Plot - Breaking Point Height ######################

BREAK = 
  ggplot(GRASS, aes(x=Soil, y=Breaking_Point, fill = Soil)) + 
  geom_violin(size = 0.5, color="black") +
  geom_point(shape=16, show.legend = FALSE, size = 2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = dt, aes(label = tukey.cld, y = 65), size=10, vjust = -0.5) +
  ylim(0, 70) +
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
  ylab("Breaking point height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3"))
BREAK

ggsave("03_Figures/Breaking.Height.png", BREAK, bg='white',
       scale = 1, width = 16, height = 6, dpi = 1500)

###### Correlation of Average Max Growth Height & Breaking Point Height ########

cor.test(GRASS$Average_Max_Height, GRASS$Breaking_Point)

# Combine both graphs #
combined = ggarrange(Avg_Max, BREAK, ncol = 1, nrow = 2)
 ggsave("03_Figures/Combined_Heights.png", combined, bg='white',
        scale = 1, width = 18, height = 12, dpi = 1500)
 