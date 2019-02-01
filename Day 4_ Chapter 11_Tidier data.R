# Day 4
# Chapter 11 : Tidier Data
# Nicole Okkers
# 01 February 2019

# 5 Primary data transformation functions: 
# 1 : arrange()_ to arrange rows
# 2 : filter()_ to filter rows/observations
# 3 : withselect()_Select variables (collumns)
# 4 : mutate()_ create new variables/collumns
# 5 : summarise()_ summarise variables (mean, median, etc)

# Load libraries
library(tidyverse)
library(lubridate) # Lubridate is a large function that allows you to work with data that is in different forms (I think...maybe stack overflow lubridate to have a better understanding) 

load("data/SACTNmonthly_v4.0.RData") # Loading data into R

SACTN <- SACTNmonthly_v4.0 # Assign a name to the data frame so that it shows up in your environment

rm(SACTNmonthly_v4.0) # Removes original data frame from the environment

# 11.1 : Comparison operators = symbols used to compare different objects
# Greater than (>); Less than (<); Greater than or equal too (>=); Less than or equal too (>=); Equal too (==); Not equal too: !=
SACTN %>%
  filter(site == "Amanzimtoti") # 1 equal too sign is maths, two equal too signs, is used for comparisons (In this data set, extract from site collumn, the site Amanzimtoti, take this site out of the data set)
# 11.2 : Logical operators
# used more broadly when making logical arguments
# )and: &) ; (or: |) ; (not: !)
SACTN %>% # (and then)
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) # Resources online for lubricate, check this out. If the date collumn was in words (Jan) instead of number (1), you simply type the word;jan

# Practice with more dates

SACTN %>% # (and then)
  filter(site == "Pollock Beach", month(date) == 9 | month(date) == 3) # Playing around with different dates

# 11.3 : Arrange
# Arrange data from smallest to biggest (Ascending) / arrange fucntion arranges collumns that you select from the lowest value to the highest value
# Can use arrange value to show the min or max value
# Can also arrange: Descending order
# Ascending order
SACTN %>%
  arrange(depth, temp)

# Descending order
SACTN %>%
  arrange(desc(temp))

# 11.4 : Filter Observations (rows) with filter
SACTN %>% # Temperature data
  filter(site == "Humewood", year(date) == 1990) 

SACTN %>% # Temperature data
  filter(site == "Humewood", year(date) == 1995) # Practice with another date: 1993

# 11.5 : select variables withselect()

try_1 <- SACTN %>%
  select(site, src, date, temp) # Selecting collumns by name (Individually)

try_2 <- SACTN %>%
  select(site:temp) # Select collumns between site and temp (Make sure its like a sequence)

try_3 <- SACTN %>%
  select(-date, -depth) # all collumns are selected, except the ones that are stated individually, excluded because of the minus sign

try_4 <- SACTN %>%
  select(-(date:depth)) # selecting collumns except those within a given sequence # Exclude from a point to a point (:) excluded everything between those two points

# 11.6 create new variables with mutate()
# Mutate is the function to create a new collumn
try_5 <- SACTN %>% # Easier to veiw variable name in the environment
  mutate(kelvin = temp + 273.15) # Kelvin is the name of the collumn that we are mutating...we are adding a new collumn called Kelvin, the temperatures will now be in the data set, using Kelvin as the unit


try_6 <- SACTN %>% # Easier to veiw variable name in the environment
  mutate(Half_Temp = temp/2) # Practicing using the mutate function

SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE))

# 11.7 
# when summarising and mutating, you first allocate a name that you want to call 
SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) # summarise function allows you to access other variables (mean etc)

# Creating additional collumns with other summaries
# na values effect overall analysis. use command: na.rm = TRUE to omit them
# Good practice to always use the command to omit NA's incase you forget to check the data set for NA's
SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), # mean added as a collumn
            sd_temp = sd(temp, na.rm = TRUE), # Standard deviation added as a collumn
            min_temp = min(temp, na.rm = TRUE), # Adding min temp as a collumn
            max_temp = max(temp, na.rm = TRUE)) # Adding max temp as a collumn


SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE), 
            min_temp = min(temp, na.rm = TRUE), 
            max_temp = max(temp, na.rm = TRUE),
            Var_temp = var(temp, na.rm = TRUE)) # Practice by adding temperature variance as a collumn












