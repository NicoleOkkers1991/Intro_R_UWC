# 03 February 2019
# Author ; Nicole Okkers
# Introduction to R Workshop Weekend Exercises
# Weekend Exercise 1: 
# Section 1: 
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, glimpse etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables (For both data sets)
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things. (Bonus Marks)

# Load Libraries
library(tidyverse)

# Load the Data
load("data/rast_feb.RData") # Only need to use "load" because data is a RData type
load("data/rast_aug.RData") 

# Exploring the data set / Examine the data set: 
# Use commands "head and tail" to examine the data set, this helps you make sure that there are no errors in your data set
head(rast_aug) # shows first 6 rows
tail(rast_feb) # shows last 6 rows
head(rast_aug, n = 50) # Shows First 50 rows
tail(rast_feb, n = 50) # Shows Last 50 rows

# Exploring the data set / Examine the data set:
# Use command 'glimpse' to examine the structure of your data set
glimpse(rast_aug) # Glimpse helps you list the variables in your data frame
glimpse(rast_feb) # Listing varibales in the rast_feb data frame

# Exploring the data set / Examine the data set:
# Using the "names" command to further examine your data
names(rast_aug) # only looking at the name of the variables (collumns)
names(rast_feb)

# Exploring the data set / Examine the data set:
# using the "nrow" and "ncol" commond to make sure R read data set properly
nrow(rast_aug) # How man rows in this data set
nrow(rast_feb) # how many rows in this data set
ncol(rast_aug) # How many collumns in this data set
ncol(rast_feb) # How many collumns in this data set

# Exploring the data set / Examine the data set:
# using the "any()" command to check the existence of particukar data
# Come back and finish this section... you saved the website go and check it our

# Create a map using the variables lat and long
# Load libraries needed to plot maps
library(ggpubr) # Libraries we use to plot maps

Map_aug <- rast_aug # assign the data set you are plotting

ggplot(data = rast_aug, aes(x = lon, y = lat)) + # This is not a good way to creat a map, a polygon is better to use to show a landmass
  geom_point() # Plotting the points/ coordinates for the lat and long variables. It is better to show this as a polygon though, so that it can create a "shape"

# Creating a land mask. this can be the first step, you can omit the geom_point command for the other data set plot
ggplot(data = rast_aug, aes(x = lon, y = lat)) + # Creating a land mask using the ggplot function
  geom_polygon(colour = "mintcream", fill = "grey0" , aes(group = season)) # useing season variable tp group the data for aestethic reasons(this way it looks like a map)

# it looks like SA so now I want to add the province borders, so that it can start looking like a map
# Load the data for the south african provinces. it is an RData type, so simply use the command: "load"
load("data/sa_provinces.RData") # Loaad sa_provinces, it was given as a resource in the data file
ggplot(data = rast_aug, aes(x = lon, y = lat)) + # Parent Line
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) + # Land Mask
  # Remember to use the province data to make the borders
  geom_path(data = sa_provinces, aes(group = group))  # when you are mapping, you can include a number of data sets to code, eg; provinces are used to create the path

# I need to explicitly state the borders I want shown on the map
# do this using the "coord_equa;" command
ggplot(data = rast_aug, aes(x = lon, y = lat)) +
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(16, 33), ylim = c(-35, -26.5), expand = 0)  # Force lon/lat extent # Setting a limit on the coordinates, in order to indicate where X and Y coordinates 'stop'
# Just ask Amieroh about this command...did you "cut" the map correctly??

# Making the map look legit- lets add ocean temperatures...I used the data provided in the data folder for the course
# Adding Ocean Temeperature
ggplot(data = rast_aug, aes(x = lon, y = lat)) +
  # The next line is the new part of the code that adds the ocean temperature
  geom_raster(data = rast_aug, aes(fill = bins)) + # The ocean temperatures # sst data will give the temp of the ocean # Bin- temperatures will fall within the bin 
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) +
  geom_path(data = sa_provinces, aes(group = group)) + # Use the province data to draw the borders of the provinces
  coord_equal(xlim = c(16, 33), ylim = c(-35, -26.5), expand = 0) 

# Creating a colour pallete for the map
# Use the
col_rast_aug <- c("#55DAD6" , "#52C9CC" , "#50B8C1" , "#4FA8B6" , "#4E97A9" , "#4C879B" , "#4A788D") # Using the links in the course reader in order to make a colour pallete for the map

# What the map looks like with the colour pallete:
ggplot(data = rast_aug, aes(x = lon, y = lat)) +
  geom_raster(data = rast_aug, aes(fill = bins)) + # filling in the data for the temperatures
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) + # Outline must be dark (Polygon gives me an outline)
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = col_rast_aug) + # Set the colour palette # Created a pallet earlier on, just reuse that code. Ran the colls_rast_aug already/ Function: Scale_fill manual
  coord_equal(xlim = c(16, 33), ylim = c(-35, -26.5), expand = 0)
