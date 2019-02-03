# Day 2 R_workshop Exercise 1
# Author: Nicole Okkers
# Date: 30 January 2019

# Excerise 1 
# Load Libaries
# Caclulating the Mean of 1 collumn in the dataset
library(tidyverse) # Download tidyverse libray
# Dataset 1 : 
datasets::iris # Download dataset 'Iris"
Iris <- datasets::iris # Adding the data set to the environment pain by assiging it a name
Iris %>%  # Choosing the dataframe
  summarise(avg_sepal_length = mean(Sepal.Length)) # Command to calculate the mean of the Seapl Length collumn

??iris # Help function to find out more about the data

# Create two Graphs for the Iris Data Set
# Graph 1 
# Hypothesis for Graph 1: Do the three different spec
plot_1 <- ggplot(Iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) + 
  geom_point() +
  geom_smooth(method = "lm") + # Adding the line of best fit
  labs(x = "Sepal.Width (cm)")
  ggtitle("Sepal.Length Versus Sepal.Width accross three different species")
plot_1

# Graph 2
# Hgypothesis for Graph 2 : Flowers with greater sepal widths will have shorter sepal lengths. Sepal width is inversely proportional to sepal length
plot_2 <- ggplot(Iris, aes(x = Sepal.Width)) + 
  geom_histogram(aes(fill = Species), position = "dodge", binwidth = 100) + # "position" makes sure that the bars are next(alongside eachother) to eachother and not on top or behind eachother
  labs(x = "Sepal Width (cm)", y ="Sepal Length (cm)") # instruction to make histogram type graph
plot_2 # This graph had some errors and didnt produce a correct Histogram 

Plot_3 <- ggplot(data = Iris, aes(x = Sepal.Width , y = Sepal.Length)) +
  geom_boxplot(aes(group = Species)(fill = Sepal.Width)) + # this is the command to produce a boxplot
  labs(x = "Sepal Width (cm)", y = "Sepal Lenght (cm)") +
  ggtitle("Box Plot to show relationship between Sepal width and sepal length of Iris Flowers")
Plot_3 # This graph is wrong, not too sure how to fix, come back to it

Plot_4 <- ggplot(data = Iris, aes( x = Sepal.Width, y = Sepal.Length))+
  geom_point() +
  geom_line(aes(group = Species)) +
  labs("Sepal Width (cm)", y = "Sepal Lenght (cm)") +
  ggtitle("Line graph showing Sepal Width and Sepal Length in centimeters")
Plot_4 # This is also not the greatest graph to use to represent this data

# For the second graph, I produced four graphs for practice and not all of the graphs 'worked'

# Dataset 2 : Rock
# Calculate the mean of 1 collumn
datasets::rock # Download dataset 'rock"
??rock # Help function to understand data

Rock <- datasets::rock # Adding the data set to the environment pain by assiging it a name
Rock %>%  # Choosing the dataframe
  summarise(avg_area = mean(area)) # Command to calculate the mean of the area collumn

# Create two Graphs for the Rock Data Set
# Graph 1 
# Hypothesis for Graph 1: The permeability of each core sample will increase as the total perimeter of pores for each core sample increase (Directly proportional)
Plot_R1 <- ggplot(data = Rock, aes( x = peri, y = perm))+ # addin the data set to the environment
  geom_point() + # attempting to create a basic figure
  geom_line(aes(group = perm)) +
  labs("Sepal Width (cm)", y = "Sepal Lenght (cm)") +
  ggtitle("Line graph showing the perimeter to permeability ratio")
Plot_R1 # Rework this graph, it doesnt work for this data set

Plot_R2 <- ggplot(Rock , aes( x = peri , y = perm , colour = area)) + # Adding the data set to the environment pain
  geom_point(colour = 'blue' ) + # Changed the points to blue
  geom_smooth(method = "lm") + # Linear Model
  labs(x = "perimeter", y ="permeability") + # Changing Lables
  ggtitle( "Graph showinge the permeability to perimeter ratio") # Adding a title to the graph
Plot_R2  # This Graph shows that as the perimeter increases, the permeability also increases
# For graph 1, I produced 2 graphs because the firat graph gave me an error. 

# Graph 2 : Rock dataset
# Hypothesis for graph 2 : The permeability of each core sample will decrease as the area of each sample increases, this is an inversely proportional relationship
Rock <- datasets::rock # Adding the data set to the environment pain by assiging it a name
Plot_R3 <- ggplot(data = Rock, aes(x = area , y = perm)) +
  geom_point(shape = 21, colour = "orangered4", fill = "white") + # assigning colours to the graph
  geom_smooth(method = 'lm') + # Adding the line of best fit to show the general trend
  labs(x = "Area (cm^2)", y = "permeability (units)") + # Adding lables
  ggtitle( "Graph showing the area to permeability ratio at each core sample")
Plot_R3
# From the graph, it can be seen that the hypothesis is accepted


# Dataset 3 : Cars
# Calculate the mean of 1 collumn
datasets::cars # Download dataset 'cars"
Cars <- datasets::cars
??cars # Help to have a better understanding of the dataset
Cars %>%  # Choosing the dataframe
  summarise(avg_speed = mean(speed)) # Command to calculate the mean of the Speed collumn

# Create two Graphs for the Cars Data Set
# Graph 1 of the Cars dataset
# Hypothesis for Graph 1C : The speed of the cars will increase as the distance increases- This is a directly proportional relationship
plot_1C <- ggplot(Cars, aes(x = dist, y = speed)) + 
  geom_point() +
  geom_smooth(method = "lm") + # Adding the line of best fit
  labs(x = "Distance (ft)" , y = "speed (mph)") +
ggtitle("Graph showing Distance Versus Speed")
plot_1C
# The map shows that the hypothesis is accepted

# Graph 2 for the Cars dataset
# Hypothesis for Graph 3: 
Plot_2C <- ggplot(data = Cars, aes(x = dist , y = speed)) +
  geom_boxplot(aes(group = dist)(fill = speed)) + # this is the command to produce a boxplot
  labs(x = "Distance (ft)", y = "Speed (mph") +
  ggtitle("Box Plot to show the relationship between the distance and speed of cars")
Plot_2c # Error with this code...Relook and try to understand


# Exercise two: Laminaria Data set
# Graph 1 for Laminaria data set
# Load Libraries
library(tidyverse)
Lam <- read_csv("data/laminaria.csv") # Read the CSV file for laminaria

# Create faceted figure

Plot_1L <- ggplot(data = Lam, aes(x = thallus_mass, y = total_length, colour = site)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~site, ncol = 2) + # This is the line that creates the facets
  labs(x = "Mass (g)", y = "Length (cm)")
Plot_1L # This Facetted graph can tell us about the Thallus mass and total length ratios at each site of this dataset

# Graph 2 for the Laminaria data set
# Hypothesis for graph 2: The thallus_mass is directly proportional to the total length

Plot_2L <- ggplot(data = Lam, aes(x = thallus_mass, y = total_length)) +
  geom_point(shape = 21, colour = "Green", fill = "white") + # assigning colours to the graph
  geom_smooth(method = 'lm') + # Adding the line of best fit to show the general trend
  labs(x = "thallus mass (kg)", y = "total length (cm)")
Plot_2L


# [A.A]
# Sjoh, only person to try different things when plotting, Nicely done
# Neat script, great comments
# NIce



