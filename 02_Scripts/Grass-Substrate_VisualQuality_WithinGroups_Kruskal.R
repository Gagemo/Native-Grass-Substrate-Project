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
                      "rstatix", "likert", "tables")
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
library(tables)

########################### Load Data ##########################################

GRASS <- read.csv("01_Data/Grass Substrate Project - Aboveground Growth.csv")

########## Organizes Substrate Treatments for Display in Graphs Later ##########

GRASS$Soil = factor(GRASS$Soil, 
                    levels = c("ProMix55BK", "NativeMix", 
                               "GardenMix", "ProMixHP"))

####################### Two-Way ANOVA Visual Shoot Data ########################

indian = filter(GRASS, Species == "Indiangrass")
sugar = filter(GRASS, Species == "Sugarcane")
wire = filter(GRASS, Species == "Wiregrass")

## Indian ##
# Check Assumptions #
model  <- lm(Visual_Quality ~ Soil, data = indian)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
indian$Soil= as.factor(indian$Soil)
indian %>% levene_test(Visual_Quality ~ Soil)

# Test for Significance #
anova_i = indian %>% kruskal_test(Visual_Quality ~ Soil) %>% 
  add_significance()
summary(anova_i)

tukey_i <- indian %>% 
  dunn_test(Visual_Quality ~ Soil) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_i

# Extract pairwise comparisons and p-values
pairwise_comparisons <- with(tukey_i, paste(group1, group2, sep = " - "))
p_adj_values <- tukey_i$p.adj

# Create a named vector for adjusted p-values
names(p_adj_values) <- pairwise_comparisons

# Use multcompLetters with the named vector
cld_i <- multcompLetters(p_adj_values, threshold = 0.05)

## Sugar ##
# Check Assumptions #
model  <- lm(Visual_Quality ~ Soil, data = sugar)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
sugar$Soil= as.factor(sugar$Soil)
sugar %>% levene_test(Visual_Quality ~ Soil)

# Test for Significance #
anova_s = sugar %>% kruskal_test(Visual_Quality ~ Soil) %>% 
  add_significance()
summary(anova_s)

tukey_s <- sugar %>% 
  dunn_test(Visual_Quality ~ Soil) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_s

# Extract pairwise comparisons and p-values
pairwise_comparisons <- with(tukey_s, paste(group1, group2, sep = " - "))
p_adj_values <- tukey_s$p.adj

# Create a named vector for adjusted p-values
names(p_adj_values) <- pairwise_comparisons

# Use multcompLetters with the named vector
cld_s <- multcompLetters(p_adj_values, threshold = 0.05)

## wiregrass ##
# Check Assumptions #
model  <- lm(Visual_Quality ~ Soil, data = wire)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
wire$Soil= as.factor(wire$Soil)
wire %>% levene_test(Visual_Quality ~ Soil)

# Test for Significance #
anova_w = wire %>% kruskal_test(Visual_Quality ~ Soil) %>% 
  add_significance()
summary(anova_w)

tukey_w <- wire %>% 
  dunn_test(Visual_Quality ~ Soil) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_w

# Extract pairwise comparisons and p-values
pairwise_comparisons <- with(tukey_w, paste(group1, group2, sep = " - "))
p_adj_values <- tukey_w$p.adj

# Create a named vector for adjusted p-values
names(p_adj_values) <- pairwise_comparisons

# Use multcompLetters with the named vector
cld_w <- multcompLetters(p_adj_values, threshold = 0.05)

tmp <- tabular(Soil ~ Visual_Quality* (mean+sd), data=sugar)
tmp
tmp <- tabular(Soil ~ Visual_Quality* (mean+sd), data=indian)
tmp
tmp <- tabular(Soil ~ Visual_Quality* (mean+sd), data=wire)
tmp

i.dt <- indian %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixHP"), ordered = TRUE))
s.dt <- sugar %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixHP"),ordered = TRUE))
w.dt <- wire %>% 
  group_by(Species, Soil) %>%
  summarise(w=mean(exp(Visual_Quality)), 
            sd = sd(exp(Visual_Quality)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() %>% 
  mutate(Soil = factor(Soil,levels = c("ProMix55BK", "NativeMix", "GardenMix", 
                                       "ProMixHP"), ordered = TRUE))

# extracting the compact letter display and adding to the Tk table
i.cld2 <- data.frame(letters = cld_i$Soil$Letters)
i.dt$tukey.cld <- i.cld2$letters
s.cld2 <- data.frame(letters = cld_s$Soil$Letters)
s.dt$tukey.cld <- s.cld2$letters
s.dt$new <- c("c", "cd", "d", "d")
w.cld2 <- data.frame(letters = cld_w$Soil$Letters)
w.dt$tukey.cld <- w.cld2$letters
w.dt$new <- c("e", "e", "e", "e")
