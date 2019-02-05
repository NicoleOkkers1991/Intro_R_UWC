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
head(rast_feb) # shows first 6 rows
tail(rast_feb) # shows last 6 rows
head(rast_aug) # shows first 6 rows
tail(rast_aug) # shows last 6 rows
head(rast_aug, n = 50) # Shows First 50 rows
tail(rast_aug, n = 50) # Shows Last 50 rows
head(rast_feb, n = 50) # Shows First 50 rows
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
attach(rast_aug) # Attach allows you to refer to any variable by name
any(season == "lat") # Does the season collumn appear in the lat collumn? No it doesn't...this is another way to explore the data, with the "any" function
attach(rast_feb) # Attach allows you to refer to any variable by name
any(lon == "season") # Is the longitude collumn found in the season collumn?...FALSW (No)

# Come back and finish this section... you saved the website go and check it our

# Create a map using the variables lat and long
# Load libraries needed to plot maps
library(ggpubr) # Libraries we use to plot maps
library(scales)
library(ggsn)

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
rast_aug_map <- ggplot(data = rast_aug, aes(x = lon, y = lat)) +
  geom_raster(data = rast_aug, aes(fill = bins)) + # Try and take out the temp collumn and use only that data. + # filling in the data for the temperatures
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) + # change the bins or something # Outline must be dark (Polygon gives me an outline)
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = col_rast_aug) + # Set the colour palette # Created a pallet earlier on, just reuse that code. Ran the colls_rast_aug already/ Function: Scale_fill manual
  coord_equal(xlim = c(16, 33), ylim = c(-35, -26.5), expand = 0) +
    labs( x = "Longitude" , y = "lattitude") + # Adding lables to the map, assigning x and y axis
  scale_x_continuous(position = "bottom") + # Put the X axis at the bottom
    annotate("text", label = "Atlantic\nOcean", # Using coordinates and colour to make lables of oceans # /n means you want a new line, so that there is a space between Atlantic and Ocean
                       x = 15, y = -33.5, # the specs for the labels for Atlantic Ocean
                       size = 4.5,
                       angle = 31,
                       colour = "royalblue1") +
    annotate("text", label = "Indian\nOcean", # Adding the Indian Ocean lable
             x = 30, y = -33.9, # Specs for the lable
             size = 4.5,
             angle = 340,
             colour = "plum") +
    scalebar(x.min = 23, x.max = 27, y.min = -34.5, y.max = -35.5, # Set location of bar
             dist = 201, height = 1.5, st.dist = 0.7, st.size = 1.5, # Set particulars
             dd2km = TRUE, model = "WGS84") + # Set appearance
    north(x.min = 23.5, x.max = 26.5, y.min = -32, y.max = -31.5, # Set location of symbol
          scale = 1.3, symbol = 16.5) +
  ggtitle("Map showing the Sea Surface temperatures around the South African Coast for the month of August")
rast_aug_map # Saving the  final Map


# Graph for the feb data
col_rast_feb <- c("#48B4B6" , "#3EA49F" , "#379488" , "#328573" , "#2F755F" , "#2C654D" , "#28563D", "#24472E")
# Using the links in the course reader in order to make a colour pallete for the map
rast_feb_map <- ggplot(data = rast_feb, aes(x = lon, y = lat)) +
  geom_raster(data = rast_feb, aes(fill = bins)) + # Try and take out the temp collumn and use only that data. + # filling in the data for the temperatures
  geom_polygon(colour = "mintcream", fill = "aliceblue", aes(group = season)) + # change the bins or something # Outline must be dark (Polygon gives me an outline)
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = col_rast_feb) + # Set the colour palette # Created a pallet earlier on, just reuse that code. Ran the colls_rast_aug already/ Function: Scale_fill manual
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs( x = "Longitude" , y = "lattitude") + # Adding lables to the map, assigning x and y axis
  scale_x_continuous(position = "top") + # Put the X axis at the bottom
  annotate("text", label = "Atlantic\nOcean", # Using coordinates and colour to make lables of oceans # /n means you want a new line, so that there is a space between Atlantic and Ocean
           x = 16, y = -31, # the specs for the labels for Atlantic Ocean
           size = 2.5,
           angle = 32,
           colour = "royalblue1") +
  annotate("text", label = "Indian\nOcean", # Adding the Indian Ocean lable
           x = 30, y = -33.9, # Specs for the lable
           size = 4.5,
           angle = 340,
           colour = "plum") +
  scalebar(x.min = 23, x.max = 27, y.min = -34.5, y.max = -35.5, # Set location of bar
           dist = 201, height = 0.5, st.dist = 0.7, st.size = 1, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 24, x.max = 26, y.min = -33, y.max = -32, # Set location of symbol
        scale = 1.3, symbol = 16.5) +
  ggtitle("Map showing the Sea Surface temperatures around the South African Coast for the month of February")
rast_feb_map # Saving the  final Map

# I will use the feb map and inset it into the aug map
Insetted_MAP <-  rast_aug_map +
annotation_custom(grob = ggplotGrob(rast_feb_map), # Always use this code to inset
                  xmin = 18.5, xmax = 27.5, # If you wanna shift postition of the inset, just play with the coordinates
                  ymin = -33.5, ymax = -28.5)
Insetted_MAP

