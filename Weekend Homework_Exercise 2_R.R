# 04 February 2019
# Author ; Nicole Okkers
# Introduction to R Workshop Weekend Exercises
# Weekend Exercise 2: 
# Section 2: You need to remember to explore the data no matter what, even when its not asked
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site) (Summarise function)
# Calculate standard error
# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset (Summary function)

# Load Libraries
library(tidyverse)
# Load data
# This is how to read a csv file in R
Ecklonia <- read_csv("data/ecklonia.csv") # Read the CSV file for laminaria as well as assinging it a name

# This is another way of reading a CSV file into R. I will convert it to a RData type file
Ecklonia_1 <- read_csv("data/ecklonia.csv") # Read the CSV file for Ecklonia
save(Ecklonia_1,file = "data/ecklonia.RData") # Converting from CSV to RData

# Exploring data
# Use commands "head and tail" to examine the data set, this helps you make sure that R has the loaded the data set correctly
head(Ecklonia_1) # shows first 6 rows
tail(Ecklonia_1) # shows last 6 rows
head(Ecklonia_1, n = 7) # Shows First 7 rows
tail(Ecklonia_1, n = 7) # Shows Last 7 rows
glimpse(Ecklonia_1) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
# Using the "names" command to further examine your data
names(Ecklonia_1) # only looking at the name of the variables (collumns)

# using the "nrow" and "ncol" commond to make sure R read data set properly
nrow(Ecklonia_1) # How man rows in this data set
ncol(Ecklonia_1) # How many collumns in this data set

# Exploring the data set / Examine the data set:
# using the "any()" command to check the existence of particukar data
# Come back and finish this section... you saved the website go and check it our

# Demonstrate dimensions of the data set
dim(Ecklonia_1) # Two dimensions to this data set

# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Graph 1: Line Graph 
# Hypothesis for Graph 1: As stipe mass increases, the epiphyte length increases
# Linear Model might display this relationship better
Line_graph <- ggplot(data = Ecklonia_1, aes(x = stipe_mass, y = epiphyte_length)) +
  geom_point(colour = "red4") + # How to start plotting a grap (This is the command for that)
  geom_line(colour = "khaki") + # Command for a line graph
  labs(x = "Stipe Mass (Kg)", y ="Epiphyte Length (cm)") + # Changing Lables
  ggtitle("Graph showing the relationship between Stipe Mass and Epiphyte Length" ) +
  theme_gray() # Playing around with the themes, used the'gray' one
Line_graph
# Answer Hypotheses

# Graph 2 : 
# Bar graph
#Hypothesis : The size of the primary blade width of the species, Ecklonia maxima will not differ accross the two different sites, Batsata Rock and Boulders Beach
Bar_graph <- ggplot(data = Ecklonia_1 , aes(fill = "Primary Blade Length"))+ # use the command "ggplot' to draw a graph. State which data set you are using
  geom_col(aes(x = site , y = primary_blade_width , group = site)) +  # Have to use command 'geom_col' becasue the demand "geom_bar" can only be used to show counts os sums of weights. This does not work for this kind of data.
  labs ( x = "Location" , y = "Primary Blade Width (cm)" ) + # Adding X and Y axis lables to the graph
  ggtitle("Bar Graph showing the size of the primary blade width of algae species, Ecklonia Maxima at two different locations") + # adding a title to the graph
  theme_classic() # I will be using the classic theme for this graph. 
Bar_graph

# Graph 3: Box Plot
# Hypothesis for graph 3: I hypothesis that there will be a difference bewteen frond length and frond mass of Ecklonia maxima at the two different sites; Boulders beach and Batsata rock
Box_plot <- ggplot(data = Ecklonia_1, aes(x = frond_mass , y = frond_length )) + # command to plot a graph with the Ecklonia data set, as well as assigning a name to the graph, so that it shows up in the environment
 geom_boxplot(aes(fill = frond_mass, group = site)) + # this is the command to produce a boxplot
  labs(x = "Frond Mass (kg)", y = "Frond Length (cm)") + # adding lables to the boxplot
  ggtitle("Box Plot to show relationship between the Frond Mass and the Frond Length of the species, Ecklonia maxima, at two different sites, Boulders beach and Batsata rock ") + # adding a title to the box plot
  theme_get() # Trying out the "get" theme for this graph
Box_plot 

# Using thed Ggarrange function to arrange graphs into one plot
# Load Library 'ggpubr'
library(ggpubr)
plot_1 <- ggarrange(Line_graph, Bar_graph, Box_plot) # install package ggpubr and then use the ggarrange to combine all of the plots
plot_1 # Shows all three graphs combined 

# Calculate the mean,max,min,median and variance for the stipe_length and stipe_diameter for each of the sites
# I will use the pipe ( %>% ) command for this part of the script. This part of the scripting we are plotting and not coding
Ecklonia_1 %>% # Tell R to use Ecklonia dataset and then ( %>% )
  group_by(site) %>% # Use the group_by function to summarise the data by sites, and then ( %>% )
  select(stipe_length , stipe_diameter) %>% # from the data set only select collumns (variables) stipe_length and stipe_diamter and then ( %>% )
  summarise(mean_SL = mean(stipe_length), # Use the summarise function to calculate the mean of the stipe_length collumn
            max_SL = max(stipe_length), # Use the summarise function to calculate the max value of the stipe_length collumn
            min_SL = min(stipe_length), # Use the summarise function to calculate the min value of the stipe_length collumn
            median_SL = median(stipe_length), # Use the summarise function to calc the median of the stipe_length collumn
            variance_SL = var(stipe_length), # Summarise to calc the variance of the stipe length collumn
            mean_SD = mean(stipe_diameter), # Summarise to calc mean of the stipe_diamter collumn
            max_SD = max(stipe_diameter), # summarise to calc max value of the stipe_diamter collumn
            min_SD = min(stipe_diameter), # Summarise to calc max value of the stipe_diameter collumn
            median_SD = median(stipe_diameter), # Summarise to calc max value of the stipe_diamter collumn
            variance_SD = var(stipe_diameter)) %>% # Summarise to calc variance of the stipe_diameter collumn and then
  na.omit() # Omit an NA

# Calculating standard error of Stipe_length
Ecklonia_1 %>% # Telling R which data set to use and then using pipe to add onto the command
  group_by(site) %>%  # Group the data by site in order to calc the standard error
  summarise(variance_SL = var(stipe_length), # Calculate the standard error
            n = n()) %>%  # Count the number samples in your sample space/data set
  mutate(se = sqrt(variance_SL/n)) # command to calculate standard error

# Calculating standard error of Stipe_diameter
Ecklonia_1 %>% # Telling R which data set to use and then using pipe to add onto the command
  group_by(site) %>%  # Group the data by site in order to calc the standard error
  summarise(variance_SD = var(stipe_diameter), # Calculate the standard error
            n = n()) %>%  # Count the number samples in your sample space/data set
  mutate(se = sqrt(variance_SD/n)) # command to calculate standard error # Make use of the "mutate" command

# Determine the min and maximum frond length and stipe length
Ecklonia_1 %>% # Tell R to use Ecklonia dataset and then ( %>% )
  group_by(site) %>% # Use the group_by function to summarise the data by sites, and then ( %>% )
  select(frond_length , stipe_length) %>% # select function to only select the stipe_length and frond_length collumns
  summarise(max_SL = max(stipe_length), # Use the summarise function to calculate the max value of the stipe_length collumn
            min_SL = min(stipe_length), # calc min value of the stipe_length collumn
            max_FL = max(frond_length), # Calc the max value of frond_length collumn
            min_FL = min(frond_length)) # Calc the min value of the frond_length collumn

# Determine the overall summary of the dataset (Summary function)
summary(Ecklonia_1) # Comman for overall summary of the Ecklonia data set


