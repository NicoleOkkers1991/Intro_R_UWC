# Day 4
# Chapter 10 : Tidy Data
# Nicole Okkers
# 01 February 2019

load("data/SACTN_mangled.RData") # Load function to read the RData file


ggplot(data = SACTN1, aes(x = date, y = temp)) + # Specify which data set you are using # AES- For aestetics
  geom_line(aes(colour = site, group = paste0(site, src))) + # To group the on the graph by allocating site and source # Every Site has a different colour # If you wanna group by two different variables, you make use of the paste0 function
  labs( x = "Date (months)", y = "Temperature (°C)") +
  ggtitle("Graph showing the monthly Temperatures of the South African coast line ")

# Gather Function (Gathering)
# Creating new data set- assign a name
# Use the gather function to combine multiple collumns into one collumn---to make tidy data {Longer than it is wide}
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")

# 10.1.2 : Spreading Should ones data be too long, meaning when individual observations are spread across multiple rows, we will
# need to use spread() to rectify the situation.
# This is usually the case when we have two or more variables that arestored within the same column, as we may see in SACTN3.
# This is not terribly common as it would require
# someone to put quite a bit of time into making a dataframe this way
# (Above description is taken from course reader)
# Spreading Function
SACTN3_tidy <- SACTN3 %>%
  spread(key = var, value = val)
# 10.2 : Separating and uniting
# Separating
# Now we will look at how to manage our columns when they contain more (or less) than one variable, but the overall
# dataframe does not need to be made wider or longer.
SACTN4a_tidy <- SACTN4a %>% # when splitting anything into one or more variables, use contactinate function
  separate(col = index, into = c("site", "src"), sep = "/ ") # Use the separate function to separate the index collumn (Contains site and source)
# Use the '/' because thats the symbol used in the data set

# Uniting 
# To unite() them we must first tell R what we want the united column to be labelled, in this case we will use ‘date’.
# We then list the columns to be united, her this is year, month, and day. Lastly we must specify if we want the united values to have a separator between them. The standard separator for date values is ‘-’.
# Unite function to combine the collumns
# Above note taken from course reader

SACTN4b_tidy <- SACTN4b %>% # Renaming/ assigning name to new data set...lable your data smartly (so that you know what is tidy data and what is not tiday data), no spaces are allowed in the new name, rather just use underscore in the name. 
  unite(year, month, day, col = "date", sep = "-") # Use hyphen to separate because thats what is used in the data set

# 10.3 Joining
# joining two different dataframes.
# Remember that one of the rules of tidy data is that only one complete dataset is saved per dataframe.
# This rule then is violated not only when additional data are stored where they don’t belong, but also when necessary data are saved elsewhere.
#  Above Notes above taken from course reader

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) # left_join Technique is the most commonly used technique
# Left_join function combines two or more data sets
# R arranged two data sets this way: R> Joining, by = c("site", "src", "date"), resulting in one large data set


# [A.A]
# Sjoh! Nice commenting
# Nicely done, neat script
# Shows good understanding
