################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
####################### Individual Height Analysis #############################
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

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixBx"))

################## One-Way ANOVA Max Height ####################################

indian = filter(GRASS, Species == "Indiangrass")
sugar = filter(GRASS, Species == "Sugarcane")
wire = filter(GRASS, Species == "Wiregrass")

i.anova = aov(Average_Max_Height ~ Soil, data = indian) %>% add_significance()
summary(i.anova)
s.anova = aov(Average_Max_Height ~ Soil, data = sugar) %>% add_significance()
summary(s.anova)
w.anova = aov(Average_Max_Height ~ Soil, data = wire) %>% add_significance()
summary(w.anova)

i.tukey = TukeyHSD(i.anova)
s.tukey = TukeyHSD(s.anova)
w.tukey = TukeyHSD(w.anova)

i.tukey.cld <- multcompLetters4(i.anova, i.tukey)
s.tukey.cld <- multcompLetters4(s.anova, s.tukey)
w.tukey.cld <- multcompLetters4(w.anova, w.tukey)

i.dt <- indian %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Average_Max_Height)), 
            sd = sd(exp(Average_Max_Height)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))
s.dt <- sugar %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Average_Max_Height)), 
            sd = sd(exp(Average_Max_Height)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"),ordered = TRUE))
s.dt$new <- c("c", "cd", "d", "d")

w.dt <- wire %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Average_Max_Height)), 
            sd = sd(exp(Average_Max_Height)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))
w.dt$new <- c("e", "e", "e", "e")

# extracting the compact letter display and adding to the Tk table
i.cld2 <- data.frame(letters = i.tukey.cld$Soil$Letters)
i.dt$tukey.cld <- i.cld2$letters
s.cld2 <- data.frame(letters = s.tukey.cld$Soil$Letters)
s.dt$tukey.cld <- s.cld2$letters
w.cld2 <- data.frame(letters = w.tukey.cld$Soil$Letters)
w.dt$tukey.cld <- w.cld2$letters

################### Box Plot - Max Height ######################
supp.labs <- c("Indiangrass","Sugarcane plumegrass","Wiregrass")
names(supp.labs) <- c("Indiangrass","Sugarcane","Wiregrass")

i.max = 
  ggplot(indian, aes(x = Soil, y = Average_Max_Height, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = i.dt, aes(label = tukey.cld, y = 95), size=8, vjust = -0.5) +
  ylim(0,105)+
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
  ylab("Max height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
i.max

s.max = 
  ggplot(sugar, aes(x = Soil, y = Average_Max_Height, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = s.dt, aes(label = new, y = 95), size=8, vjust = -0.5) +
  ylim(0,105)+
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
  ylab("Max height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
s.max

w.max = 
  ggplot(wire, aes(x = Soil, y = Average_Max_Height, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  facet_grid(. ~ Species, labeller = labeller(Species = supp.labs)) +
  geom_text(data = w.dt, aes(label = new, y = 95), size=8, vjust = -0.5) +
  ylim(0,105)+
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
  ylab("Max height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
w.max

################################################################################
################## Breaking Point Height #######################################
################################################################################

i.anova = aov(Breaking_Point ~ Soil, data = indian) %>% add_significance()
summary(i.anova)
s.anova = aov(Breaking_Point ~ Soil, data = sugar) %>% add_significance()
summary(s.anova)
w.anova = aov(Breaking_Point ~ Soil, data = wire) %>% add_significance()
summary(w.anova)

i.tukey = TukeyHSD(i.anova)
s.tukey = TukeyHSD(s.anova)
w.tukey = TukeyHSD(w.anova)

i.tukey.cld <- multcompLetters4(i.anova, i.tukey)
s.tukey.cld <- multcompLetters4(s.anova, s.tukey)
w.tukey.cld <- multcompLetters4(w.anova, w.tukey)

i.dt <- indian %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Breaking_Point)), 
            sd = sd(exp(Breaking_Point)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"), ordered = TRUE))

s.dt <- sugar %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Breaking_Point)), 
            sd = sd(exp(Breaking_Point)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixBx"),ordered = TRUE))

w.dt <- wire %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Breaking_Point)), 
            sd = sd(exp(Breaking_Point)) / sqrt(n())) %>%
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
s.dt$new <- c("g", "gh", "gh", "h")
w.cld2 <- data.frame(letters = w.tukey.cld$Soil$Letters)
w.dt$tukey.cld <- w.cld2$letters
w.dt$new <- c("i", "i", "i", "i")

################### Box Plot - Breaking Heights  ######################

i.break = 
  ggplot(indian, aes(x = Soil, y = Breaking_Point, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = i.dt, aes(label = new, y = 65), size=8, vjust = -0.5) +
  ylim(0,70)+
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
  ylab("Breaking height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
i.break

s.break = 
  ggplot(sugar, aes(x = Soil, y = Breaking_Point, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = s.dt, aes(label = new, y = 65), size=8, vjust = -0.5) +
  ylim(0,70)+
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
  ylab("Breaking height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
s.break

w.break = 
  ggplot(wire, aes(x = Soil, y = Breaking_Point, fill = Soil)) + 
  geom_violin(size = 0.5, color="black", show.legend = TRUE) +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = w.dt, aes(label = new, y = 65), size=8, vjust = -0.5) +
  ylim(0,70)+
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
  ylab("Breaking height (cm)") +
  xlab("") +
  scale_fill_manual(name = "Substrate Media", 
                    labels = c("Pro-Mix 55BK", "Native Mix", 
                               "Garden Mix", "Pro-Mix Bx"), 
                    values=c("indianred", "seagreen1", 
                             "gold3", "slateblue3")) 
w.break

# Combine both graphs #
combined = ggarrange(i.max, s.max, w.max, i.break, s.break, w.break, 
                     ncol = 3, nrow = 2, common.legend = TRUE, legend="bottom")
combined
ggsave("03_Figures/Combined_Heights_withinGroups.png", combined, bg='white',
       scale = 1, width = 18, height = 12, dpi = 1500)

