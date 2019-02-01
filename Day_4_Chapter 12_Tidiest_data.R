# Day 4
# Chapter 12 : Tidiest Data
# Nicole Okkers
# 01 February 2019

# Group-by() function : Calculating statistics based on the different grouping variables within our data

# Load libraries
library(tidyverse)
library(lubridate)


load("data/SACTNmonthly_v4.0.RData") # Loading data from a .RData


SACTN <- SACTNmonthly_v4.0 # assing a name to data frame to appear in the environment


rm(SACTNmonthly_v4.0) # Remove original data frame

#12.1 Group observations (rows) by variables (columns) with group_by()
# Group by function is found in the tidyverse package
# Group by depth
SACTN_depth <- SACTN %>%
  group_by(depth) # Group_by groupd the depth

# Apparently this is a bad way
SACTN_depth_mean <- SACTN_depth %>% # Calculating the mean temp from the depth 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            count = n())

#  Apparently This is the right way
SACTN_depth_mean <- SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
  count = n())
SACTN_depth_mean

# Visualise the results
SACTN_depth_mean

# Why does the relationship between depth and temperature look so odd?
ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  ggtitle( " Graph showing the depth and mean temperatures for the SACTN dataframe") + # Adding Graph title
  labs( x = "Depth (m)", y = "Mean Temperature (°C)") # Adding lables to the graph

# 12.3 Group all of the functions
SACTN_30_years <- SACTN %>% # Assigning a new name to the data frame so that it will pop up in the environments
  group_by(site, src) %>% # Group by function to group  same sites and same source collumn
  filter(n() > 360) # this command is telling R to filter out 30 years...360 months = 30 years

# 12.4
# First create a character vector containing the desired sites
# Use contactinate (CANNOT SPELL) too create the list of sites that will be filtered
# Need to create a set using concatinate
selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") # This command is used in line 61...This is the way to filter more than 1 site at time. 
                                                                          # This line is vitally important and needed to filter more than one site
# Then calculate the statistics
# Filter by more than 1 site: 
# Filter by one site: SACTN %>% 
                # filter(site == " Port Nolloth')
SACTN %>%
  filter(site %in% selected_sites) %>% # Filtering by four sites (Not one site) # use the concatinate set made in previous command
  group_by(site, src) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE))
