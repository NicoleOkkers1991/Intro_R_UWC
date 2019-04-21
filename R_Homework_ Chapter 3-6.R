# R_ Homework_Biostats_2019
# 19 April 2019
# Chapter 3 Exercise
# Author: Nicole Okkers

# Load Libraries for the exercises
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(fitdistrplus)
library(logspline)
library(plotly)

# Exercise 3.6.1
summary(chicks$weight) # Using the summarise function 
Task_1 <- chicks %>% # Assigning a name 
  filter(Time < 10) %>% 
  group_by(Diet, Time) %>% 
  summarise(min_wt = min(weight), # Use the quantile function to break up the quartiles
            qrt1_wt = quantile(weight, p = 0.25), # calculating the first quartile
            med_wt = median(weight), # Second quartile (which is also the mean)
            qrt3_wt = quantile(weight, p = 0.75), # Thid quarter of data
            max_wt = max(weight)) # These things are very sensitive to outliers

# Chapter 4 Exercise
# Exercise 4.3.1

# Load buuilt-in data set: Orange
datasets::Orange 
Task2 <- Orange # Assigning a name to the dataset

# Graph 1
# Histogram (Frequency with count along the y axis)
Task_2 <- ggplot(data = Orange, aes(x = "", y = age, fill = Tree)) + # Command to start plotting a graph
  geom_bar(width = 1, stat = "identity") + # I will attempt to plot a stacked bar showing the relative proportions for the ages of trees
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions for ages of the trees", # Adding lables and titles to the graph
       x = NULL, y = "Age of Trees") + # X- Axis
  theme_classic() + scale_color_few() + # Using a classic theme
  scale_fill_few()
Task_2 # Graph 1 for exercise 4.3.1

# Graph 2 
# Using attenu Data set
datasets::attenu
Task_3 <- attenu # Assinging name to data set so that it shows up in the environment

Task_3 <- ggplot(data = attenu, aes(x = accel)) + # Command to plot a graph
  stat_ecdf() + # Doing a ECDF graph for the accelerations of the earthquakes
  labs(title = "The Joyner–Boore Attenuation Data",
       subtitle = "ECDF Showing the accelerations measured at the 23 stations", # adding lables and titles to graph and axes
       x = "Acceleration (numeric	 Peak acceleration (g))", # x axise
       y = "Relative contribution") + theme_cleveland() # Y- axis and theme
Task_3 # Graph 2 for exercise 4.3.1

# Graoh 3
# Ussing BOD Data set
datasets::BOD
Task_4 <- Orange # Assigning a name 

Task_4 <- ggplot(data = BOD, aes(x = demand)) + # Command to plot a Graph
  geom_histogram(aes(y = ..density..), # Command to plot a histogram
                 position = 'identity', binwidth = 1, # Command for binwidth and position
                 colour = "black", fill = "purple", alpha = 0.6) + # Assigning colours
  labs(title = "Biochemical Oxygen Demand", # Title and subtitle
       subtitle = "A frequency histogram",
       x = "Demand (mg/l)", # X axis
       y = "Count") + theme_classic2() # adding theme to the graph and y axis title

Task_4 # Graph 3 for exercise 4.3.1

# Graph 4: 
# Using attenu data set

Task_5 <- Orange # Assigning name to show up in the environment

Task_5 <- ggplot(data = Orange, aes(x = age, y = circumference, fill = Tree)) + # assign variables to the box plot
  geom_boxplot(show.legend = FALSE, notch = FALSE) + theme_pubclean() + # assigning "parameters" for graph
  labs( title = "Box Plot showing the age amd circumference of Orange Trees", # Title
        y = "Circumference (mm)") + # Y title
  theme(axis.text.x = element_text(face = "italic")) # Graph theme
Task_5 # Graph 4 for exercise 4.3.1

# All $ graphs for Exercise 4.3.1

ggarrange(Task_2, Task_3, Task_5, Task_5, nrow = 2, ncol = 2, labels = "AUTO") # Arranging all the graphs in one nice neat image

# Chapter 5 Exercises
# Choose two different datasets and plot them as histograms with density curves overlayed. 
# Label them with the distribution they appear to be and stitch them together with ggarrange().

# Histogram overlayed by density curve graph number 1
# Load built in data set "cars"
datasets::cars
Task_6 <- cars # assigning a name to the data set so thst it shows up in the data set
Task_6 <- ggplot(cars, aes(x = dist)) + # assigning aes and data set for the graph that I want to plot
  geom_histogram(aes(y = ..density..), binwidth = 3,
                 colour = "black", fill = "orange", alpha = 0.6) + # Assigning colours) 
  labs(title = "The distance of cars", # Title and subtitle
       subtitle = "A histogram overlayed by density plot",
       x = "Distance (numeric	 Stopping distance (ft))", # X axis
       y = "Count") + theme_classic2() + # adding theme to the graph
  geom_density() # Command to add a denisty plot
Task_6

# Histogram overlayed by density plot number 2

datasets::Orange
Task_7 <- Orange # Using buil-in data set orange
Task_7 <- Orange %>% 
  select(Tree, circumference) # Using the select function to tidy up the data

# Plot the graph
Task_7 <- ggplot(Orange, aes(x = circumference)) + # assigning aes and data set for the graph that I want to plot
  geom_histogram(aes(y = ..density..), binwidth = 3,
                 colour = "black", fill = "green", alpha = 0.6) + # Assigning colours) 
  labs(title = "Histogram showing the circumference of a selection of trees", # Title and subtitle
       subtitle = "A histogram overlayed by density plot",
       x = "Circumference (mm)", # X axis
       y = "Count of Trees") + theme_classic2() + # adding theme to the graph
  geom_density() # Command to add a denisty plot
Task_7

# Chapter 6 Execises
# 6.7 Exercises
# 6.7.1 Exercise 1
# Find or create your own normally distributed data and think of a hypothesis you could use 
# a t-test for. Write out the hypothesis, test it, and write a one sentence conclusion for 
# it. Provide all of the code used to accomplish this.
 
# Example for normally distributed data. 
# Load Data
# Will use built-in data set "ecklonia"
ecklonia <- read_csv("data/ecklonia.csv") %>% # Command to load the data 
  gather(key = "variable", value = "value", -species, -site, -ID) # Gather takes multiple columns and collapses into key-value pairs, duplicating all other columns as needed. You use gather() when you notice that you have columns that are not variables

# Visulise the data. Do this so that you can get a better understanding of the data , so that you can formulate a hyothesis
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) + # Command to plot a graph. setting the aes
  geom_boxplot() + # box plot is a good graph to see the distribution of the data
  coord_flip() # Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal. This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y
# Make observations about the data
# The measurments ate Basta Rock 

# Formulate a hypothesis
# Stipe masses between the two sites appear to be similar. Lets explore the data a bit 
# filter the data
ecklonia_sub <- ecklonia %>%  
  filter(variable == "stipe_mass") # Command to filter out the data
# No draw a figure to a figure to graphically represent the data

ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) + # command to lpot a graph and set the aes
  geom_boxplot() + # Command to plot a boxplot
  coord_flip() + # Comman to cartesians coordinates
  labs(y = "stipe mass (kg)", x = "") + # Command to add lables to the graph
  theme(axis.text.y = element_blank(), # adding a theme to the graph
        axis.ticks.y = element_blank()) 

# Are the stipe masses at Basta Rock greater than the stipe masses at Basta Rock? 

# H0: Stipe masses at Basta Rock are not greater than the stipe masses at Boulders Beach
# H1: Stpe masses at Basta Rock are greater than stipe masses at boulders beach

# What kind of T test should we use for this data? 
# Two samples that we are comparing therfore I will use a t test
# I want to know if the stipe mass at Basta rock is greater than the stie mass at boulders breach and I want to know if the two samples differ
# This means that I will use a one-side t-test
# Check assumptions

# These are the assumptions: 
# the dependent variable must be continuous (i.e. it is measured at the interval or ratio level),
# the observations in the groups being compared are independent of each other,
# the data are normally distributed, and
# that the data are homoscedastic, and in particular, that there are no outliers

# Depenedent variable for thsi data et is continous
# Observations are independent of one another

# Is this data normally distributed and homoscedastic?

# Explore the data set
ecklonia_sub %>% 
  group_by(site) %>% # Command to group the data set by site
  summarise(stipe_mass_var = two_assum(value)[1], # summarise command to explore the data set
            stipe_mass_norm = two_assum(value)[2])

# This data is normally distributed and homoscedastic

# Run the t_test analysis: 
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater") # This iis the traditional output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater") # Dataframe output

# The null hypothesis is rejected because p-value ≤ 0.05
# The alternative hpothesis (H1) is accepted. 

# 6.7.2 Exercise 2
# Do the same as Exercise 1, but for probability data.

# Creating a data set using Lions and whether or not they 
Lions <- data_frame(c(98, 105, 45, 25), ncol = 2) 
colnames(Lions) <- c("yes", "no")
rownames(Lions) <- c("Springbok", "Eland")
Lions

# Deciding on hypothesis
# When the probability matches the population
prop.test(x = 45, n = 100, p = 0.5)

# When it doesn't
prop.test(x = 33, n = 100, p = 0.5) 

# Two samples that I want to compare
prop.test(Lions) # prop.test can be used for testing the null that the proportions (probabilities of success) in several groups are the same, or that they equal certain given values.

# Two-sided test
# Explicitly state two-sided test
prop.test(Lions, alternative = "two.sided")

# The Springbok is a bit less than the Eland
prop.test(Lions, alternative = "less")

# The Springbok is a bit more than the Elandc
prop.test(Lions, alternative = "greater")

# T_test analysis
t.test (data = Lions, var.equal = TRUE, alternative = "greater") # This iis the traditional output
compare_means(data = Lions, method = "t.test", var.equal = TRUE, alternative = "greater") # Dataframe output

