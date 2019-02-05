# 04 February 2019
# Author ; Nicole Okkers
# Introduction to R Workshop Weekend Exercises
# Weekend Exercise 4: 
# Section 4: open section (Can make your own data set)
# Make use of any two built in datasets:
# Make use of the summarise, select, group_by functions
# Create at least two visualisations that were not done in the Intro R workshop (Density Plots: Geom_will show you the different functions)

# Load Libaries
library(tidyverse)
Beaver_2 <- datasets::beaver2 # using the built in data set beaver 2 and assigning it a name so that it shows up in the environment pain
# Exploring the data
# For Data set 1
Beaver_2 %>% # Tell R to use Ecklonia dataset and then ( %>% )
  group_by(day) %>% # Use the group_by function to summarise the data by sites, and then ( %>% )
  select(time , temp) %>% # from the data set only select collumns (variables) temp and time and then ( %>% )
  summarise(mean_Time = mean(time), # Use the summarise function to calculate the mean of the time collumn
            max_Time = max(time), # Use the summarise function to calculate the max value of the time collumn
            min_Time = min(time), # Use the summarise function to calculate the min value of the time  collumn
            median_Time = median(time), # Use the summarise function to calc the median of the time  collumn
            variance_Time = var(time), # Summarise to calc the variance of the time  collumn
            mean_Temp = mean(temp), # Summarise to calc mean of the temp collumn
            max_Temp = max(temp), # summarise to calc max value of the temp collumn
            min_Temp = min(temp), # Summarise to calc max value of the temp collumn
            median_Temp = median(temp), # Summarise to calc max value of the temp collumn
            variance_Temp = var(temp)) %>% # Summarise to calc variance of the temp collumn and then
  na.omit() # Omit an NA
summary(Beaver_2) # Summary of the Beaver_2 data set


# Exploring the data
# For Data set 2
Iris_data <- datasets::iris # using the built in data set beaver 2 and assigning it a name so that it shows up in the environment pain
# Exploring the data

Iris_data %>% # Tell R to use Ecklonia dataset and then ( %>% )
  group_by(Species) %>% # Use the group_by function to summarise the data by sites, and then ( %>% )
  select(Sepal.Length , Petal.Length) %>% # from the data set only select collumns (variables) Sepal.Length and Petal.Length and then ( %>% )
  summarise(mean_SL = mean(Sepal.Length), # Use the summarise function to calculate the mean of the Sepal.Length collumn
            max_SL = max(Sepal.Length), # Use the summarise function to calculate the max value of the Sepal.Length collumn
            min_SL = min(Sepal.Length), # Use the summarise function to calculate the min value of the Sepal.Lengthcollumn
            median_SL = median(Sepal.Length), # Use the summarise function to calc the median of the Sepal.Length collumn
            variance_SL = var(Sepal.Length), # Summarise to calc the variance of the Sepal.Length collumn
            mean_PL = mean(Petal.Length), # Summarise to calc mean of the Petal.Length collumn
            max_SW = max(Petal.Length), # summarise to calc max value of the Petal.Length collumn
            min_SW = min(Petal.Length), # Summarise to calc max value of the Petal.Length collumn
            median_SW = median(Petal.Length), # Summarise to calc max value of the Petal.Length collumn
            variance_SW = var(Petal.Length)) # Summarise to calc variance of the Petal.Length collumn and then
 

# Plot 1 Bar graph that is different to the collumn bar i have in the section 2
Plot_1_ex4 <- Bar_graph <- ggplot(data = Beaver_2 , aes(fill = "Temperature"))+ # use the command "ggplot' to draw a graph. State which data set you are using
  geom_bar(aes(x = time , group = temp)) +  # Have to use command 'geom_bar" instead of "geom_col"
  labs ( x = "Time (s)" , y = "Temperature" ) + # Adding X and Y axis lables to the graph
  ggtitle("Bar Graph showing the Temperature and the timees it was recorded for the Beaver dataset") + # adding a title to the graph
  theme_minimal() # I will be using the classic theme for this graph. 
Plot_1_ex4

# Plot 2, attempting a density plot
# Load libraries
# Density Plot
library(ggplot2)
Plot_2_ex4 <- ggplot(data = Iris_data , aes( x= Sepal.Length)) +
  geom_density() +
labs ( x = "Sepal Length (cm)" , y = "Density" ) + # Adding X and Y axis lables to the graph
  ggtitle("Density plot for the sepal length of the Iris data set") + # adding a title to the graph
  theme_pubr() # I will be using the classic theme for this graph. 
Plot_2_ex4




