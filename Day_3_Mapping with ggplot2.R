# Day 4
# Chapter 7 : Mapping with ggplot 2
# Nicole Okkers
# 31 January 2019

# Load Libraries
library(tidyverse) 
library(ggpubr) # Libraries we use to plot

# Load Data
# Only need to use "load" because its a RData type file
load("data/south_africa_coast.RData") 
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

# Choose which SST product you would like to use
sst <- MUR_low_res # sst- Sea Surface Temperature

cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a") # The colour pallette we will use for ocean temperature

ChickWeight <- datasets::ChickWeight 

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point() 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point() + # This creates a map # Plotting points a long the pain to produce map, use coordinate data for this
  labs (x = "Longitude" ,  y = "Lattitude")
  # add a title here after class today

# Landmask
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "red", fill = "grey70" , aes(group = group)) + # The land mask # group is the name of the collumn(Grouping by the collumn)
  labs (x = "Longitude" ,  y = "Lattitude")
# Grey 70 is a colour  
# You can modify the command to show different colours

# Borders
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + # Parent Line
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + # The province borders # when you are mapping, you can include a number of data sets to code, eg; provinces are used to create the path
labs (x = "Longitude" ,  y = "Lattitude") # Adding Lables 
# Province data will make the path because data was collected at sites in provinces, where as the coast data was only taken around the coast and the polygon just froms an outline

# Force lon/lat
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) + # Force lon/lat extent # Setting a limit on the coordinates, in order to indicate where X and Y coordinates 'stop'
  labs (x = "Longitude" ,  y = "Lattitude") 

# Ocean Temperature
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + # aes- specify what you put on x and y axis
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures # sst data will give the temp of the ocean # Bin- temperatures will fall within the bin 
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + # Use the province data to draw the borders of the provinces
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs (x = "Longitude" ,  y = "Lattitude") 
 # Do not lable collumns the same name as a function

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # Raster command to bring in the sea surface data
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + # Outline must be black (Polygon gives me an outline)
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette # Created a pallet earlier on, just reuse that code. Ran the colls11 already/ Function: Scale_fill manual
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs (x = "Longitude" ,  y = "Lattitude") 

# Adding coastal Pixels 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), # Makes the different pixels (Tiles)
            colour = "white", size = 0.1) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) + # Look at your data set to tell you about the range and what limits to use
  labs (x = "Longitude" ,  y = "Lattitude") 

# Final Map :
final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), #  this is how to Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4)) # Fine tune position of legend
final_map

ggsave(plot = final_map, "data/map_complete.pdf", height = 6, width = 9) # This is how to save the map in files






