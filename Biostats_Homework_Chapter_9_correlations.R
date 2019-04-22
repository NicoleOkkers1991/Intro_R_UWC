# Chapter 9 questions: Correlations
# 9.6 Exercises
# 9.6.1 Exercise 1
# Produce a heat map using ggplot2.

# Load Libraries
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)
library(reshape2)

# Choose data set
# WIll use Ecklonia data set
# Using a built -in data set
Heat_map_Q9 <- melt(ecklonia_pearson) # Load data set and assign a name

# Explore data set
head(Heat_map_Q9) # shows first 6 rows
tail(Heat_map_Q9) # shows last 6 rows
head(Heat_map_Q9, n = 9) # Shows First 9 rows
tail(Heat_map_Q9, n = 9) # Shows Last 9 rows
glimpse(Heat_map_Q9) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(Heat_map_Q9) # baconSACTN_DAY1) # How man rows in this data set
ncol(Heat_map_Q9) # How many collumns in this data set
summarise(Heat_map_Q9) # Summarise data set to check out means
summary(Heat_map_Q9) # Summary command

# Plotting a heat map
ggplot(Heat_map_Q9, aes(x=Var1, y=Var2, fill=value)) + # Command to plot graph using Heat_map data set
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "indianred", name = "Pearson correlation") + # Setting parameters for the graph
  theme(axis.text.x = element_text(angle = 45, vjust = 1, # assigning theme to graph
                                   size = 12, hjust = 1)) +
  ggtitle("Heat Map for the Ecklonia data set" ) # COmmand for title



