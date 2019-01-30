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

# Dataset 2 :
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
# Hypothesis for graph 2 : 



