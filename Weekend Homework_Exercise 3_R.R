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
SACTN_DAY1 %>% 
  sum # come back to this one 

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

SACTN4a_tidy_attempt1 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")
