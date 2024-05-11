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

indian = filter(GRASS, Species == "Indiangrass")
sugar = filter(GRASS, Species == "Sugarcane")
wire = filter(GRASS, Species == "Wiregrass")

i.anova = aov(Visual_Quality ~ Soil, data = indian) %>% add_significance()
summary(i.anova)
s.anova = aov(Visual_Quality ~ Soil, data = sugar) %>% add_significance()
summary(s.anova)
w.anova = aov(Visual_Quality ~ Soil, data = wire) %>% add_significance()
summary(w.anova)

i.tukey = TukeyHSD(i.anova)
s.tukey = TukeyHSD(s.anova)
w.tukey = TukeyHSD(w.anova)

i.tukey.cld <- multcompLetters4(i.anova, i.tukey)
s.tukey.cld <- multcompLetters4(s.anova, s.tukey)
w.tukey.cld <- multcompLetters4(w.anova, w.tukey)

i.dt <- indian %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))
s.dt <- sugar %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"),ordered = TRUE))
w.dt <- wire %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
i.cld2 <- data.frame(letters = i.tukey.cld$Soil$Letters)
i.dt$tukey.cld <- i.cld2$letters
s.cld2 <- data.frame(letters = s.tukey.cld$Soil$Letters)
s.dt$tukey.cld <- s.cld2$letters
s.dt$new <- c("c", "cd", "d", "d")
w.cld2 <- data.frame(letters = w.tukey.cld$Soil$Letters)
w.dt$tukey.cld <- w.cld2$letters
w.dt$new <- c("e", "e", "e", "e")

################### Box Plot - Shoot Visual Quality ######################
supp.labs <- c("Indiangrass","Sugarcane plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

i.max = 
  ggplot(indian, aes(x = Soil, y = Visual_Quality, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = i.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =  element_text(margin = unit(c(0, 0, 0, 0), "mm"),
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
        axis.ticks = element_blank()) +
  ylab("Shoot visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
i.max

s.max = 
  ggplot(sugar, aes(x = Soil, y = Visual_Quality, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = s.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y =element_blank(),
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
        axis.ticks = element_blank())+
  ylab("Shoot visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
s.max

w.max = 
  ggplot(wire, aes(x = Soil, y = Visual_Quality, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = w.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y =element_blank(),
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
        axis.ticks = element_blank())+
  ylab("Shoot visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
w.max

################################################################################
################## Root Quality ################################################
################################################################################

i.anova = aov(Root_Quality_Scale ~ Soil, data = indian) %>% add_significance()
summary(i.anova)
s.anova = aov(Root_Quality_Scale ~ Soil, data = sugar) %>% add_significance()
summary(s.anova)
w.anova = aov(Root_Quality_Scale ~ Soil, data = wire) %>% add_significance()
summary(w.anova)

i.tukey = TukeyHSD(i.anova)
s.tukey = TukeyHSD(s.anova)
w.tukey = TukeyHSD(w.anova)

i.tukey.cld <- multcompLetters4(i.anova, i.tukey)
s.tukey.cld <- multcompLetters4(s.anova, s.tukey)
w.tukey.cld <- multcompLetters4(w.anova, w.tukey)

i.dt <- indian %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Root_Quality_Scale)), 
            sd = sd(exp(Root_Quality_Scale)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))

s.dt <- sugar %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Root_Quality_Scale)), 
            sd = sd(exp(Root_Quality_Scale)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"),ordered = TRUE))

w.dt <- wire %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Root_Quality_Scale)), 
            sd = sd(exp(Root_Quality_Scale)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))


# extracting the compact letter display and adding to the Tk table
i.cld2 <- data.frame(letters = i.tukey.cld$Soil$Letters)
i.dt$tukey.cld <- i.cld2$letters
i.dt$new <- c("f", "f", "f", "f")
s.cld2 <- data.frame(letters = s.tukey.cld$Soil$Letters)
s.dt$tukey.cld <- s.cld2$letters
s.dt$new <- c("g", "h", "h", "h")
w.cld2 <- data.frame(letters = w.tukey.cld$Soil$Letters)
w.dt$tukey.cld <- w.cld2$letters
w.dt$new <- c("i", "i", "i", "i")

################### Box Plot - Root Mass  ######################

i.break = 
  ggplot(indian, aes(x = Soil, y = Root_Quality_Scale, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = i.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =  element_text(margin = unit(c(0, 0, 0, 0), "mm"),
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
        axis.ticks.x=element_blank(),
        axis.ticks = element_blank()) +
  ylab("Root visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
i.break

s.break = 
  ggplot(sugar, aes(x = Soil, y = Root_Quality_Scale, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = s.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y =element_blank(),
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
        axis.ticks = element_blank())+
  ylab("Root visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
s.break

w.break = 
  ggplot(wire, aes(x = Soil, y = Root_Quality_Scale, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = TRUE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = w.dt, aes(label = tukey.cld, y = 4), size=9, vjust = -0.5) +
  ylim(0,4.5)+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y =element_blank(),
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
        axis.ticks = element_blank(),
        legend.position="bottom")+
  ylab("Root visual quality") +
  xlab("") +
  scale_fill_manual(name = "", 
                    labels = c("Pro-Mix 55BK", "Native mix", 
                               "Garden mix", "Pro-Mix BX"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
w.break

# Combine both graphs #
combined = ggarrange(i.max, s.max, w.max, i.break, s.break, w.break, 
                     ncol = 3, nrow = 2, common.legend = TRUE, legend="bottom")
combined
ggsave("03_Figures/Combined_Visual_withinGroups.png", combined, bg='white',
       scale = 1, width = 18, height = 12, dpi = 1500)

#################### Correlation Test - Shoot/Root Mass ########################

cor.test(GRASS$Visual_Quality, GRASS$Root_Quality_Scale)

