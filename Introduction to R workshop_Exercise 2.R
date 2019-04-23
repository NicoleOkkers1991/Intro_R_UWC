# 05 February 2019
# Author ; Nicole Okkers
# Intro to R workshop: Exercise 2

# Part 1:
# Load Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(dplyr)
load("data/SACTNmonthly_v4.0.RData") # Load the SACTNmonthly_v4.0 data set
SACTN <- SACTNmonthly_v4.0 # Assing a name to the data frame so that it shows up in the environment

# Explore Data set
head(SACTN) # shows first 6 rows
tail(SACTN) # shows last 6 rows
head(SACTN, n = 60 ) # Shows First 7 rows
tail(SACTN, n = 60) # Shows Last 7 rows
glimpse(SACTN) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(SACTN) # only looking at the name of the variables (collumns)
nrow(SACTN) # How man rows in this data set
ncol(SACTN) # How many collumns in this data set
dim(SACTN) # To show dimensions for the data frame
summary(SACTN) # Overall summary of the data set, to make sure that R has read the data set properly

SACTN_1 <- SACTNmonthly_v4.0 %>%  # Assinging a new name to the data frame
filter(src == "KZNSB" ) %>% # use the pipe function because we ae coding and not plotting. # This command is filtering out the collumn we need, in this case; KZNSB
  separate(col = date, into = c("year", "month", "day" , sep ="-")) %>%  # The command to spearate the date collumn into a year, month and day collumns
  group_by(site, year) %>%  # Group by is after separate command, because there is no "year" collumn until you have separated the collumns # Need to group by site as well as grouping by the year. other wise will give you every point for all of the days
  summarise(Average_temp = mean(temp)) %>%  # Calculating the mean temperature
  # there are NA data recorded in the data frame, I must omit these
  na.omit()

# Plotting the Graph
# Assing a name to the graph so that it is saved in the environment pain
SACTN_Final_plot <- ggplot(data = SACTN_1, aes(x = year , y = Average_temp)) + # ggplot command to plot the graph for the SACTN_1 data set
  geom_line(aes(group = site), colour = "yellowgreen") + # command geom_line to create a line graph and grouping the data by sites
  facet_wrap(~site, ncol = 5) +  # There are 5 collumns in AJ's graph, therefore I need facet_wrap with 5 collumns
  scale_x_discrete(name = "Year", breaks = c(1980, 2000), labels = c("1980", "2000")) + # discrete for X axis because of the non-numeric data entries (Displayed as characters)
  scale_y_continuous(name = "Temperature (°C)", breaks = c(20, 22 ,24), labels = c("20", "22", "24"), limits = c(20, 24)) + # Continuous for the y axis because of the numeric values # define the limit according to AJ's graph
  labs( x = "Year", y = "Temperature (°C)") + # Adding lables to the graph
  ggtitle("KZNSB : Series of annual means") # Adding titles to the graph
SACTN_Final_plot # Final Plot

# Part 2 :
LAM <- read_csv("data/laminaria.csv") # Load data as well as assigning it a name in order to show up in the environment

LAM_False_bay_PLOT_A <- LAM %>%  # telling R which data set to use and assigning a name
  filter( region == "FB") # Filter command to filter out the data for the false bay regions

# Exploring the dat set:
head(LAM) # shows first 6 rows
tail(LAM) # shows last 6 rows
head(LAM, n = 30 ) # Shows First 7 rows
tail(LAM, n = 30) # Shows Last 7 rows
glimpse(LAM) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(LAM) # only looking at the name of the variables (collumns)
nrow(LAM) # How man rows in this data set
ncol(LAM) # How many collumns in this data set
dim(LAM) # To show dimensions for the data frame
summary(LAM) # Overall summary of the data set, to make sure that R has read the data set properly

LAM_False_bay_PLOT_A_1 <- ggplot(LAM_False_bay_PLOT_A, aes(x = blade_length, y = blade_weight, colour = site)) +
  scale_color_brewer(palette = "Accent") + # The scale_color_brewer has set palletes, that only have 8 codes for a colour, this data set needs a palette with 9 colours
  geom_point() + # Command to plot points to draw a graph
  geom_line() + # Comman to plot graph
  facet_wrap(~site, ncol = 3) + # Facet_wrap command to combine the different graphs. note that there are 3 collumns, according to AJ's dat
  labs(x = "Blade_Length (cm)", y = "Blade_Mass (kg)") + # command to add labels to the graph
  ggtitle(" A crazy graph for some data for False Bay sites") # adding titles to the 
LAM_False_bay_PLOT_A_1

# The last graph for Roman Rock does not display. This is because the scale_color_brewer has a set palette for only 8 colours. This data set needs 9 colours. This is the problem.
# I am going to create my own colour palette, using the link in the course reader and use that as the colour palette

col_LAM_False_bay_PLOT_A <- c("#E37D85", "#D986A9", "#BC97C5", "#91A9D3", "#62B8CE", "#4AC2B9", "#60C898", "#8CC976", "#BBC55C") # Using the links in the course reader in order to make a colour pallete for the map
# R does have palettes that have colours for more than 9 variables...thats another way to go about this problem

LAM_False_bay_PLOT_B <- ggplot(LAM_False_bay_PLOT_A, aes(x = blade_length, y = blade_weight, colour = site, (colours = col_LAM_False_bay_PLOT_A)))+ # Plot new graph with new colour palette 
                                 geom_point() + # Command for points
                                 geom_line() + # Command to connect points with a line
                                 facet_wrap(~site, ncol = 3) + # Facet_wrap with 3 collumns according to AJ's graph
                                 labs(x = "Blade length (cm)", y = "Blade mass (kg)") + # Adding x and y labels
                                 ggtitle("A crazy graph for some data for False Bay sites") # Adding a graph title
LAM_False_bay_PLOT_B # Final Plot

LAM_Graph_A <- annotate_figure(LAM_False_bay_PLOT_A_1, # Command to annotate the graph
                               top = text_grob("A", color = "maroon" , # Specifying which colour to annotate in
                                               face = "bold" , size = 12)) # Specs for annotation
                               
LAM_Graph_B <- annotate_figure(LAM_False_bay_PLOT_B, # Command to annoate the second graph
                               top = text_grob("B" , color = "hotpink1" , # Specifying colour for annotation
                                               face = "bold" , size = 12)) # Specifying colour and font for annotation
                                               
# Load Libraries
library(ggpubr) # Need this library for ggarrange command
plot_Combined <- ggarrange(LAM_Graph_A, LAM_Graph_B) # install package ggpubr and then use the ggarrange to combine all of the plots
plot_Combined # Shows all three graphs combined 


# Part 3
# Tooth Growth Data
Tooth_Growth_data <- datasets::ToothGrowth %>% # loading the built-in data set ToothGrowth and assigning a name so that it shows up in the environment pain
  group_by(supp, dose) %>% # use group_by function to group supp and dose...making the data tidier
  summarise(mean_length = mean(len), # Use the summarise function to calculate the Standard Deviation and Mean Length
            Standard_dev_length = sd(len)) # calculating standard deviation
Tooth_Growth_data

# Exploring data
head(Tooth_Growth_data) # shows first 6 rows
tail(Tooth_Growth_data) # shows last 6 rows
head(Tooth_Growth_data, n = 2 ) # Shows First 7 rows
tail(Tooth_Growth_data, n = 2) # Shows Last 7 rows
glimpse(Tooth_Growth_data) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(Tooth_Growth_data) # only looking at the name of the variables (collumns)
nrow(Tooth_Growth_data) # How man rows in this data set
ncol(Tooth_Growth_data) # How many collumns in this data set
dim(Tooth_Growth_data) # To show dimensions for the data frame
summary(Tooth_Growth_data) # Overall summary of the data set, to make sure that R has read the data set properly


Tooth_Growth_Plot <- ggplot(Tooth_Growth_data, aes(x= dose, y = mean_length, fill = supp)) + # usinh ggplot to plot the collumn graph. This is probably a collumn that has standard error bars
geom_col(aes(fill= supp), position = "dodge", colour = "firebrick4") + # command to plot a collumn bar
  geom_errorbar(aes(ymin = mean_length - Standard_dev_length, # Error bars added using command geom_errorbar
                ymax = mean_length + Standard_dev_length), # setting ymax and ymin
                position = "dodge") +  # This command is responsible for alligning the position of error bars
  labs( x = "Dose (mg/d)", y = "Length of Tooth (mm)") + # Adding Labels to the graph
  ggtitle("Graph showing the rate bewteen vitamin c dosages and tooth length")
Tooth_Growth_Plot

# END











