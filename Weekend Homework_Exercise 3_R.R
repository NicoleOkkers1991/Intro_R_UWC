# 04 February 2019
# Author ; Nicole Okkers
# Introduction to R Workshop Weekend Exercises
# Weekend Exercise 3: 
# Section 3: (this what you did on the first day with AJ)
# Make use of the SACTN_day1 data: 
# Here create a graph showing temperature variation between sites
# Select all the temperatures recorded at the site Port Nolloth during August or September.
# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
# Calculate the average temperature by depth (Maybe by site)
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script


# Load Libraries
library(tidyverse)
library(lubridate) # Need to load lubridate package so that R can work with the different formats of data
SACTN_DAY1 <- read_csv("data/SACTN_day_1.csv") # Read the CSV file for SACTN_DAY_1

# Explore the data
head(SACTN_DAY1) # shows first 6 rows
tail(SACTN_DAY1) # shows last 6 rows
head(SACTN_DAY1, n = 75) # Shows First 75 rows
tail(SACTN_DAY1, n = 75) # Shows Last 75 rows
glimpse(SACTN_DAY1) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(SACTN_DAY1) # only looking at the name of the variables (collumns)
nrow(SACTN_DAY1) # How man rows in this data set
ncol(SACTN_DAY1) # How many collumns in this data set

# Graph Showing Temperature variation

SACTN_DAY_1_TEMP_graph_attempt_1 <- ggplot(data = SACTN_DAY1, aes(x = date, y = temp, group_by(site))) +
  geom_point(colour = "darkblue") + # How to start plotting a grap (This is the command for that)
  geom_smooth(method = "lm") + # Command for a straight line regression
  labs(x = "Date (months)", y ="Temp. (°C)") + # Changing Lables
  ggtitle("Graph showing the Temperature variation at the different sites for the SACTN_DAY_1 data" ) +
  theme_gray() # Playing around with the themes, used the'gray' one
SACTN_DAY_1_TEMP_graph_attempt_1

SACTN_DAY_1_TEMP_graph_attempt_2 <- ggplot(data = SACTN_DAY1, aes(x = date, y = temp)) +
  geom_point(colour = "red4") + # How to start plotting a grap (This is the command for that)
  geom_line(colour = "khaki") + # Command for a line graph
  geom_smooth(method  = "lm") + # Adding line of best fit
  labs(x = "Date (months)" , y = "Temp. (°C)") + # Changing Lables
  ggtitle("Graph showing the Temperature variation at the different sites for the SACTN_DAY_1 data" ) +
  theme_classic2() # Playing around with the themes, used the'gray' one
SACTN_DAY_1_TEMP_graph_attempt_2
# Both graphs (above) show the same variation of temperatures...I am sure there is a better way to draw this graph

# Select all the temperatures recorded at the site Port Nolloth during August or September.
SACTN_DAY1 %>% # Command to tell R what data to use and we use a pipe sign because we are coding and not plotting
  filter(site == "Port Nolloth" , month(date) == 08 | month(date) == 09) # Use logical operators (or, and, not) to allocate which months and use the filter function to show only Port Nolloth


# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
SACTN_DAY1 %>% 
  filter(site == "Port Nolloth" , year(date) == 1994) # Use the filter function to filter out the year 1994 as well as filtering by site


# Calculate the average temperature by depth (Maybe by site)
# There is no depth collumn
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script
# Load Libraries
library(tidyverse) # Tidyverse is the package that we use to clean up our data
load("data/SACTN_mangled.RData") # Load the dataset provided. The data was a RData type, so only need to use the command 'load'
# There were five different data sets loaded into the environment

# SACTN1 is an example of a tidy data set. every data set should be that tidy. 
# The data is tidy enough to make a simple plot like the code below: 
ggplot(data = SACTN1, aes(x = date, y = temp)) + # ggplot is the command to start plotting a graph, with the straight forward SACTN1 dataset
  geom_line(aes(colour = site, group = paste0(site, src))) + #To group the on the graph by allocating site and source # Every Site has a different colour # If you wanna group by two different variables, you make use of the paste0 function
  labs(x = "", y = "Temperature (°C)", colour = "Site") + # adding lables to the graph
  ggtitle("Graph showing the monthly Temperatures of the South African coast line ") + # Adding a title to the graph
theme_minimal() # trying out new themes


# SACTN2 data set is missing the collumn 'src' and temperatures are denoted (written as) according to the way they were collected
# The next bit of code is referred to as " Gathering"
SACTN2_tidy_attempt1 <- SACTN2 %>% 
  gather(KZNSB, SAWS, DEA, key = "src", value = "temp") # Gather the src collumns back together # Playing around with my own order 
# Now it looks like a tidy data set
# Temps are NA because actual values werent recorded

# Data should never be wider than it is long. This is the case for the nect data set, SACTN3
# SACTN3 data set shows that there are two or more variables that are stored within same collumn. 
# The next bit of code can be referred to as "Spreading"
SACTN3_tidy_attempt1 <- SACTN3 %>% 
  spread(key = var, value = val) # tell R what the name of the collumn is that contains more than 1 variable. # Tell r what collumn needs to be spreed

# Site and souce are two separate variables, so should be in two diferrent collumns
# We will use the "separate" command to separate the 1 collumn into 2 separate collumns
SACTN4a_tidy_attempt1 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") # This data appears accross two different data sets

# Unit collumns that have been split, but should be in one collumn
# For example: year, month and day recorded in separate collumns
SACTN4b_tidy_attempt1 <- SACTN4b %>% 
  unite(year, month, day, col ="date", sep = "-") # Uniting th year, month and day collumns into one collumn, using the "unite" command

# Joining : combining two data frames into one
SACTN4_tidy_attempt <- left_join(SACTN4a_tidy_attempt1 , SACTN4b_tidy_attempt1, by = c("site", "src", "date"))

# Tidier data
# Load libraries
library(tidyverse)
library(lubridate)

# Load the data 
load("data/SACTNmonthly_v4.0.RData")

SACTN <- SACTNmonthly_v4.0 # assigning a shorter name to the data frame

rm(SACTNmonthly_v4.0) # Command to remove the original data frame name

SACTN %>% 
  filter(site = "Amanzimtoti") # error, site shouldnt be named, use ==
# Making the above data tidier
SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) # This is the correct command to filter the site by month...you need lubridate library to access this command

# Using the "arrange" command to arrange the data
SACTN %>% 
  arrange(depth, temp) # This command helps you arrange the data

SACTN %>% 
  arrange(desc(temp)) # Arranging the data from big to small (descending)
# The next command will filter the observations (rows) of the data set
SACTN %>% 
  filter(site == "Humewood", year(date) == 1990) # Filter out the year 1990, need lubridate library for this

humewood_90s <- SACTN %>%  # Assing a name to the new data frame so that it appears in the enironment pain
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))

SACTN %>% 
  filter(site == "Port Nolloth", # command for which site to filter
         src == "DEA", # need to specify the source
         temp <= 11 | # Temperatures at or below 11°C 
           is.na(temp)) # or include th missing values 

# Selecting variable/collumns from the data, with "select" command
SACTN %>% 
  select(site:temp) # Select the collumns between site and tem. Select these collumns like a sequence

#
SACTN %>% 
  select(-date, -depth) # Select all collumns excpet the ones state in the command ie: depth and date

# Select all columns except those within a given sequence

SACTN %>% 
  select(-(date:depth)) # take note of where the "-" is...outside the bracket!

# Use select function to re order the individual collumns
SACTN %>% 
  select(temp, src, date, site)


SACTN %>% 
  select(type, src, everything()) # "everything function allos you to grab all collumns


SACTN %>% 
  select(temp:type, everything(), -src) # Attempting all of the commands in one go

# Creating new variables from already existing variables
SACTN %>% 
  mutate(kelvin = temp + 273.15)

# Command to summarise the variables within your data fram
SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), # calc mean temp
            sd_temp = sd(temp, na.rm = TRUE), # calc SD for temp col
            min_temp = min(temp, na.rm = TRUE), # Calc min temp
            max_temp = max(temp, na.rm = TRUE)) # Calc max temp        

# Tidiest Data
# Load libraries
library(tidyverse)
library(lubridate)


load("data/SACTNmonthly_v4.0.RData") # Load data

SACTN <- SACTNmonthly_v4.0 # Assing to environment pain

rm(SACTNmonthly_v4.0) # Remove the original data frame

# Using "group_by" to tidy the data
# Group by depth
SACTN_depth <- SACTN %>% 
  group_by(depth)

# calculating a new variable, using an existing variable
SACTN_depth_mean <- SACTN_depth %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), # calc mean temp by depth
            count = n())

SACTN_depth_mean # To see the results

# Plot the graph for the data
ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Grouping by multiple variables
# Create groupings based on temperatures and depth
SACTN_temp_group <- SACTN %>% 
  group_by(round(temp), depth) # Creating groupings that are based on temp and depth variables

SACTN_src_group <- SACTN %>% # Groupings based on date and source variables
  group_by(src, date)

# Create groupings based on date and depth
SACTN_date_group <- SACTN %>% # Groupings based on date and depth
  group_by(date, depth)

# command to ungroup the data
SACTN_ungroup <- SACTN_date_group %>%  # assinging name so that it appears in the environment pain
  ungroup()

# Pipe function
SACTN_depth_mean_2 <- SACTN %>% # base data frame and assigning name into environment pain
  group_by(depth) %>% # Group by depth variable/collumn
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculating means
            count = n()) # counting observations

# Group all of the functions
SACTN_30_years <- SACTN %>% # Assigning a new name to the data frame so that it will pop up in the environments
  group_by(site, src) %>% # Group by function to group  same sites and same source collumn
  filter(n() > 360) # this command is telling R to filter out 30 years...360 months = 30 years

# calculating anomaly data
SACTN_anom <- SACTN %>% # assinging new name
  group_by(site, src) %>%  # Group by site and src collumns
  mutate(anom = temp - mean(temp, na.rm = T)) %>%  # Mutate/ create a new collumn
  select(site:date, anom, depth, type) %>%  # Selecting which collumns you want 
  ungroup()

SACTN %>% # Selecting two sites and then calc mean
  filter(site == "Paternoster" | site == "Oudekraal") %>% # Filter by sites and src
  group_by(site, src) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calc mean 
            sd_temp = sd(temp, na.rm = TRUE)) # calc standard deviation

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

# Pipe into ggplot2
# Putting all of the commands together and having a good time with R
SACTN %>% # Choose starting dataframe
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>% # Select sites
  select(-depth, -type) %>% # Remove depth and type columns
  mutate(month = month(date), # Create month column
         index = paste(site, src, sep = "/ ")) %>% # Create individual site column
  group_by(index, month) %>% # Group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculate mean temperature
            sd_temp = sd(temp, na.rm = TRUE)) %>% # Calculate standard deviation
  ggplot(aes(x = month, y = mean_temp)) + # Begin with ggplot, switch from '%>%' to '+'
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.4) + # Create a ribbon
  geom_line(col = "red", size = 0.3) + # Create lines within ribbon
  facet_wrap(~index) + # Facet by individual sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + # Control x axis ticks
  labs(x = "Month", y = "Temperature (°C)") + # Change labels
  theme_dark() # Set theme

# Renaming variables
SACTN %>% 
  rename(source = src)

# Creating a new data frame from a newly created collumn
SACTN %>%  # use the transmute command
  transmute(kelvin = temp + 273.15)

SACTN %>% 
  group_by(site, src) %>% # using the goup_by function to build your data set
  transmute(kelvin = temp + 273.15)

# Counting the number of observations within the data set
SACTN_n <- SACTN %>% 
  group_by(site, src) %>% 
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>% 
  arrange(mean_temp) %>% 
  ungroup() %>% 
  select(mean_temp) %>% 
  unique()

ggplot(data = SACTN_n, aes(x = 1:nrow(SACTN_n), y = mean_temp)) +
  geom_point() +
  labs(x = "", y = "Temperature (°C)") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

SACTN_n <- SACTN %>% 
  group_by(site, src) %>% 
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>% 
  ungroup() %>% 
  select(mean_temp) %>% 
  group_by(mean_temp) %>% 
  summarise(count = n())
ggplot(data = SACTN_n, aes(x = 1:nrow(SACTN_n), y = mean_temp)) +
  geom_point(aes(size = count)) +
  labs(x = "", y = "Temperature (°C)") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

SACTN %>% 
  group_by(site, src) %>% 
  summarise(mean_temp = round(mean(temp, na.rm = T))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = mean_temp)) +
  geom_density(fill = "seagreen", alpha = 0.6) +
  labs(x = "Temperature (°C)")

# Selecting obersvations using the command "slice"

SACTN %>% 
  slice(10010:10020) # slicing a sequence of rows

SACTN %>%
  slice(c(1,8,19,24,3,400)) # command to slice specific rows


SACTN %>% 
  slice(-(c(1,8,4))) # slice rows except 1,8 and 4

# Slice all rows except a sequence
SACTN %>% 
  slice(-(1:1000)) # show all rows excpet the sequence that is stated in the command

# The top 5 variable sites as measured by SD
SACTN %>% 
  group_by(site, src) %>%  # group_by site and src
  summarise(sd_temp = sd(temp, na.rm = T)) %>%  # summarise command to calc SD
  ungroup() %>% 
  arrange(desc(sd_temp)) %>% 
  slice(1:5)

# Summary Functions to calc stats for variables
SACTN %>% 
  na.omit() %>%  # leave out any NA recorded data
  group_by(src) %>% # Group by src
  summarise(count = n(), 
            count_15 = sum(temp > 15)) %>%  # calc the sample space
  mutate(prop_15 = count_15/count) %>%  # create a new collumn
  arrange(prop_15) # arrange the data

# New Age...THE EFFICIENT WAY TO CODE
read_csv("data/SACTN_data.csv") %>% # Load the SACTN Day 1 data
  mutate(month = month(date)) %>% # Then create a month abbreviation column
  group_by(site, month) %>% # Then group by sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Lastly calculate the mean
            sd_temp = sd(temp, na.rm = TRUE)) %>% # and the SD
  ggplot(aes(x = month, y = mean_temp)) + # Begin ggplot
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.4) + # Create a ribbon
  geom_point(aes(colour = site)) + # Create dots
  geom_line(aes(colour = site, group = site)) + # Create lines
  labs(x = "", y = "Temperature (°C)", colour = "Site") # Change labels