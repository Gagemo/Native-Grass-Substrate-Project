############################ Installs Packages if Needed #####################################################

list.of.packages <- c("ggplot2", "tidyverse", "agricolae", "labelled", "vegan")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  #####################################################

library(ggplot2)
library(tidyverse)
library(vegan)
library(agricolae)

# Clears environment
rm(list=ls(all=TRUE))

# Clears history
cat("\014") 

set.seed(2)


################### Grass-Substrate Project - Dry Mass Data ###############################################################

### Load Data ###
GRASS_MASS <- read.csv("Grass Substrate Project - Data_MASS.csv")

################################################ Dry Shoot Weight ###########################################

GRASS_MASS$Soil<- factor(GRASS_MASS$Soil, levels = c("ProMix-55BK", "A1", "B1", "ProMix-Bx"))

# Box Plots Dry Shoot Weight#
ggplot(GRASS_MASS, aes(x=Soil, y=Shoot.Weight, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(size = 12, face="bold"),
        axis.text.x = element_text(face="bold")) +
  ylab("Shoot Weight (g)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))

# Two-Way ANOVA Dry Shoot Weight #
two.way <- aov(Shoot.Weight ~ Species + Soil + Species*Soil, data = GRASS_MASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Dry Shoot Weight #
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Shoot.Weight ~ Species + Soil, data = GRASS_MASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Multiple comparisons Dry Shoot Weight #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat


# One-Way ANOVA Dry Shoot Weight #
indian = filter(GRASS_MASS, Species == "Indiangrass")
wire = filter(GRASS_MASS, Species == "Wiregrass")
sugar = filter(GRASS_MASS, Species == "Sugarcane")

i.one.way <- aov(Shoot.Weight ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Shoot.Weight ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Shoot.Weight ~ Soil, data = sugar)
summary(s.one.way)


############################## Dry Root Weight ################################################################

# Box Plots  Dry Root Weight #
ggplot(GRASS_MASS, aes(x=Soil, y=Root.Weight, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(size = 12, face="bold"),
        axis.text.x = element_text(face="bold")) +
  ylab("Root Weight (g)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))

# Two-Way ANOVA Dry Root Weight #
two.way <- aov(Root.Weight ~ Species + Soil + Species*Soil, data = GRASS_MASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Dry Root Weight #
cor.test(GRASS_MASS$Shoot.Weight, GRASS_MASS$Root.Weight)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Root.Weight ~ Species + Soil, data = GRASS_MASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Multiple comparisons Dry Root Weight #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat

# One-Way ANOVA Dry Root Weight #
i.one.way <- aov(Root.Weight ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Root.Weight ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Breaking.Point ~ Soil, data = sugar)
summary(s.one.way)

