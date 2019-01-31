# Day 4
# Chapter 8 : Mapping with style
# Nicole Okkers
# 31 January 2019

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)

# Load Africa map
load("data/africa_map.RData")

# Access default maps within tidyverse package
ggplot() +
  borders() +# The global shape file
  coord_equal() # Equal sizing for lon/lat

# How to focus on the area that is just SA...We wont ever use a map of the whole world...lets zoom into the part of the map that we only need
sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") + # Borders create lines around every region
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) + # Force lon/lat extent # Putting limitations on coordinates for x and y to zoom into the part that we have # Can get coordinates for the place you need from the internet
  labs(x = "Longitude" ,  y = "Lattitude")
sa_1
# coord_equal comman...get the values for the limits from the lat and long coordinates in the data set

# Specific Lables
sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", # Using coordinates and colour to make lables of oceans # /n means you want a new line, so that there is a space between Atlantic and Ocean
           x = 15.1, y = -32.0,
           size = 5.0,
           angle = 30,
           colour = "mediumaquamarine") +
  annotate("text", label = "Indian\nOcean",
           x = 33.2, y = -34.2,
           size = 5.0,
           angle = 330,
           colour = "peachpuff2")
sa_2
