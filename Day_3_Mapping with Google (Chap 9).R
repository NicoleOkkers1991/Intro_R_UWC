## Day 4
# Chapter 9 : Mapping with Google
# Nicole Okkers
# 31 January 2019

# Loading Libraries
library(tidyverse)
library(ggmap)

# Load Data
load("data/cape_point_sites.RData")

cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
# load("data/cape_point.RData")



