# 16 April 2019
# Biostats Day 3
# Author: Nicole Okkers
# Chapter 9 : Correlations

#Load libraries

library(tidyverse)
library(ggpubr)
library(corrplot)
library(RColorBrewer)

#Load data from csv
ecklonia <- read_csv("data/ecklonia.csv") # Command for csv

#Explore data
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID) 

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the defaut settings.
# They are only shown here to illustrate that they exist.
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, #this is the code always used
         use = "everything", method = "pearson") #can be changed but not normally done




ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Spearman rank correlation c 
# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

# kendal Rank Correlation

ecklonia_norm <- ecklonia_sub %>% #new name to data
  gather(key = "variable") %>% #collects it all under one
  group_by(variable) %>%  #group it by variable
  summarise(variable_norm = as.numeric(shapiro.test(value)[2])) #tests for normality
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", #outcomes in environment as a value
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))
#paste0 saves a value that can then be added as a value

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

#heat map would be a good one to use
corrplot(ecklonia_pearson, method = "circle") 


#HEATMAP
colour = colorRampPalette(brewer.pal(8, "PuOr"))(25)
heatmap(ecklonia_pearson, scale="column",
        Colv = NA, Rowv = NA, col = colour)


