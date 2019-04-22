# Chapter 7 questions
# 7.4 Exercises
# 7.4.1 Exercise 1
# Here is bunch of data for pigs raised on different diets. 
# The experiment is similar to the chicken one. 
# Does feed type have an effect on the mass of pigs at the end of the experiment?
 
# Load Libraries
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)


# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# Explore data set
head(bacon) # shows first 6 rows
tail(bacon) # shows last 6 rows
head(bacon, n = 5) # Shows First 5 rows
tail(bacon, n = 5) # Shows Last 5 rows
glimpse(bacon) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(bacon) # baconSACTN_DAY1) # How man rows in this data set
ncol(bacon) # How many collumns in this data set
summarise(bacon)

#ANOVA Test

# QUESTION: Does the feed type have an effect on the mass of pigs at the end of the experiment?
# H0: There is NO difference in the mass of pigs at the end of the experiments after being fed different diets
# H1: There IS a difference in the mass of the pigs at the end of the experiments after being fed oifferent diets


pigs.aov <- aov(mass ~ feed, data = bacon) # Using bacon data set and command for ANOVA
summary(pigs.aov) # Summarise data set to visualise everything

# Pr < 0.05, so we reject the null hypothesis
# so, there is a difference in the mass of the pigs after being fed one of the four differenrt diets

# There is a difference.
# I want to know which feed has the effect/ results in the higher mass/ which one is different

# Visualise the data with a boxplot

ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) + # COmmand for boxplot for bacon data
  geom_boxplot(notch = TRUE)

# Perform a Tekuey anakysis
# Tukey 

TukeyHSD(pigs.aov)
# p-adj < 0.05 for all feeds, this means  all diets are different and the difference is significant
# lower and upper intervals do not go across zero in all cases

# Visualise this 

plot(TukeyHSD(pigs.aov)) # Command to visualise the Tukey anaylsis

# 7.4.2 Exercise 2
# Construct suitable null and alternative hypotheses for the built-in ToothGrowth data, 
# and test your hypotheses using an ANOVA.

teeth_7 <- datasets::ToothGrowth # Load dataset and assign name so that it shows up in the environment pane

# Explore data set: 

head(teeth_7) # shows first 6 rows
tail(teeth_7) # shows last 6 rows
head(teeth_7, n = 5) # Shows First 5 rows
tail(teeth_7, n = 5) # Shows Last 5 rows
glimpse(teeth_7) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(teeth_7) # baconSACTN_DAY1) # How man rows in this data set
ncol(teeth_7) # How many collumns in this data set
summarise(teeth_7) # Summarise data set to check out means

# Perform ANOVA
# question: Will the difference in doses of Vitamin C have an effect on tooth growth in terms of tooth lenght in guinea pigs? 
# H0: There is NO difference in tooth lengths of guinea pigs receiving different doses of vitamin C
# H1: There IS a difference in tooth lengths of guinea pigs receiving different doses of vitamin C

# I must tidy data set in order to  Filter out only Vitamin C doses 

teeth_vc_7 <- ToothGrowth %>%  # Tidying data and assigning it a new name
  filter(supp == "VC")

# Explore new data set: 
head(teeth_vc_7) # shows first 6 rows
tail(teeth_vc_7) # shows last 6 rows
head(teeth_vc_7, n = 2) # Shows First 2 rows
tail(teeth_vc_7, n = 2) # Shows Last 2 rows
glimpse(teeth_vc_7) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(teeth_vc_7) # baconSACTN_DAY1) # How man rows in this data set
ncol(teeth_vc_7) # How many collumns in this data set
summarise(teeth_vc_7) # Summarise data set to check out means




teeth.aov <- aov(len ~ as.factor(dose), data = teeth_vc_7) # Command to perfomr an ANOVA
summary(teeth.aov)
# pr < 0.05, so I will reject the null hypothesis
# This means there is a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# I want to know which dosage causes the difference/results in the longer tooth growth

# Visualise this using a boxplot 

ggplot(data = teeth_vc, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) + # COmmand for boxplot
  geom_boxplot(notch = FALSE)
# this boxplot suggests they are different


#Perform a Tukey test
# Tukey 

TukeyHSD(teeth.aov) # Command for Tukey test
# p-adj < 0.05, very low for all. This means all doses are different and this difference is significant.  
# lower and uper intervals does not cross zero

# Visualise this 

plot(TukeyHSD(teeth.aov)) # Command to Plot Tukey Analysis


# 7.4.3 Exercise 3
# Find or generate your own data that lend themselves to being analysed by a two-way ANOVA. 
# Generate suitable hypotheses about your data, and analyse it. 
# Supplement your analysis by providing a suitable descriptive statistical summary and graph(s) of your 
# data.

# I will use the toothgowth data

teeth_7.4.3 <- datasets::ToothGrowth

# Explore data set
head(teeth_7.4.3) # shows first 6 rows
tail(teeth_7.4.3) # shows last 6 rows
head(teeth_7.4.3, n = 9) # Shows First 9 rows
tail(teeth_7.4.3, n = 9) # Shows Last 9 rows
glimpse(teeth_7.4.3) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(teeth_7.4.3) # baconSACTN_DAY1) # How man rows in this data set
ncol(teeth_7.4.3) # How many collumns in this data set
summarise(teeth_7.4.3) # Summarise data set to check out means
summary(teeth_7.4.3) # Summary command

# Formulating hypotheses for ANOVA
# H0: interactions between the dose and suplement have NO effect on the tooth lenght
# H1: interactions between the dose and supplement DO have an effect on the tooth lenght

# Explore the data further
# looking at only length by supplement
summary(aov(len ~ supp, data = teeth)) # Use summary command for this

# Perfomr a Tikey analysis
TukeyHSD((aov(len ~ supp, data = teeth))) # COmmand for Tukey

plot(TukeyHSD((aov(len ~ supp, data = teeth))))# Plot the Tukey analysis

# dose was done in previous example

# now to look at interactions BETWEEN factors
summary(aov(len ~ supp * as.factor(dose), data = teeth)) # Use the summary function for this  
# pr < 0.05, so this means that I will reject the null hypothesis
# Therfore interactions between the dose and supplement have an effcet on the tooth lenght. 


TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))) # Dose or supplement have the effect? I want to know which one

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))) # Plot Tukey analysis
# combinations do not cros zero, this shows which combinations of supplement and dose have the most effect on the lenght of the tooth 





 