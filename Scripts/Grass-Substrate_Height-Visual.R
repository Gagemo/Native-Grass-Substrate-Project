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

################### Grass-Substrate Project -  Height & Quality Data ###############################################################

### Load Data ###
GRASS <- read.csv("Grass Substrate Project - Data.csv")

################################################ Average Max Growth Heights ###########################################

# Organizes the Substrates so they display nicely in grahs later #
GRASS$Soil<- factor(GRASS$Soil, levels = c("ProMix-55BK", "A1", "B1", "ProMix-Bx"))

# Box Plots Average Max Growth Heights #
ggplot(GRASS, aes(x=Soil, y=Average.Max.Height, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(size = 12, face="bold"),
        axis.text.x = element_text(face="bold")) +
  ylab("Average Max Height (cm)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))

# Two-Way ANOVA Average Max Growth Heights #

two.way <- aov(Average.Max.Height ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Average Max Growth Heights #

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Average.Max.Height ~ Species + Soil, data = GRASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Multiple comparisons Average Max Growth Heights #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat


# One-Way ANOVA Average Max Growth Heights #
indian = filter(GRASS, Species == "Indiangrass")
wire = filter(GRASS, Species == "Wiregrass")
sugar = filter(GRASS, Species == "Sugarcane")

i.one.way <- aov(Average.Max.Height ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Average.Max.Height ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Average.Max.Height ~ Soil, data = sugar)
summary(s.one.way)


############################## Breaking Point Heights ################################################################

# Box Plots  Breaking Point Heights #
ggplot(GRASS, aes(x=Soil, y=Breaking.Point, fill = Soil)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_grid(. ~ Species) +
  theme_bw() +
  theme(axis.text = element_text(face="bold"), 
        strip.text.x = element_text(size = 12, face="bold"),
        axis.title.y =  element_text(size = 12, face="bold"),
        axis.text.x = element_text(face="bold")) +
  ylab("Breaking Point Height (cm)") +
  xlab("") +
  guides(fill=guide_legend(title="Substrate Media")) + 
  scale_fill_manual(values=c("indianred", "seagreen1", "gold3", "slateblue3"))

# Two-Way ANOVA Breaking Point Heights #

two.way <- aov(Breaking.Point ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Breaking Point Heights #
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Breaking.Point ~ Species + Soil, data = GRASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

# Multiple comparisons Breaking Point Heights #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat

# One-Way ANOVA Breaking Point Heights #
i.one.way <- aov(Breaking.Point ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Breaking.Point ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Breaking.Point ~ Soil, data = sugar)
summary(s.one.way)

# Correlation of Average Max Growth Heights & Breaking Point Heights #
cor.test(GRASS$Average.Max.Height, GRASS$Breaking.Point)

############################## Shoot Visual Quality ################################################################
# Box Plots Shoot Visual Quality  #
ggplot(GRASS, aes(x=Soil, y=Visual.Quality, color = Soil)) + 
  geom_boxplot() +
  facet_grid(. ~ Species) +
  theme_bw()

# Two-Way ANOVA Shoot Visual Quality #
two.way <- aov(Visual.Quality ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Shoot Visual Quality #
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Visual.Quality ~ Species + Soil, data = GRASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Multiple comparisons Shoot Visual Quality #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat

# One-Way ANOVA Shoot Visual Quality #
i.one.way <- aov(Visual.Quality ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Visual.Quality ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Visual.Quality ~ Soil, data = sugar)
summary(s.one.way)

############################## Root Visual Quality  ################################################################

# Two-Way ANOVA Root Visual Quality #
two.way <- aov(Root.Quality.Scale ~ Species + Soil + Species*Soil, data = GRASS)
summary(two.way)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# TukeyHSD Test Root Visual Quality #
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

tukey.plot.aov<-aov(Root.Quality.Scale ~ Species + Soil, data = GRASS)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Multiple comparisons Root Visual Quality #
HSD.stat = HSD.test(two.way,trt = c("Species","Soil")) #HSD Tukey 
HSD.stat

# One-Way ANOVA Root Visual Quality #
i.one.way <- aov(Root.Quality.Scale ~ Soil, data = indian)
summary(i.one.way)

w.one.way <- aov(Root.Quality.Scale ~ Soil, data = wire)
summary(w.one.way)

s.one.way <- aov(Root.Quality.Scale ~ Soil, data = sugar)
summary(s.one.way)
