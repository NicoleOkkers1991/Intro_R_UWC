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

# Scale Bars
# Scale bar function is not a fixed function...so you can play around with the numbers until scale workd with map
sa_3 <- sa_2 + # Assing map sa_2 to Sa_3---sa_2 is kind of a parent line
  scalebar(x.min = 32, x.max = 26, y.min = -34, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

# Insetting Maps
sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map), # Always use this code to inset
                    xmin = 20.9, xmax = 26.9, # If you wanna shift postition of the inset, just play with the coordinates
                    ymin = -30, ymax = -24)
sa_4

#[A.A]
# Neat script
# Tried new things
# Great commenting, nicely dome