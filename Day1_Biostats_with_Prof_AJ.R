# 09 April 2019
# Biostats Day 1
# Morning session ( 08:30 - 12 noon)
# Author: Nicole Okkers

# loads the tidyverse functions; it contains the 'as_tibble()' function
library(tidyverse) # Loading the tidyverse library

# the 'ChickWeight' data are built into R;
# here we assign it as a tibble to an object named 'chicks'
chicks <- as_tibble(ChickWeight)

# Exploring the data
head(chicks) # Exploring data set using head command
tail(chicks) # Exploring data set using tail command
tail(chicks, n = 2)# Tail showing last two data sets
tail (chicks, n = 21) # Showing last 21 rows
colnames(chicks) # Function that lists the names of the collumns/variables
names <- colnames(chicks) # assigning name to collumns
names [3] # Showing the name of the third collumn
summary(chicks) # Showing the summary of the data set (stats summary)
dim(chicks) # Checking dimensions of the chick data sets
glimpse(chicks)# Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
names(chicks) # only looking at the name of the variables (collumns)
nrow(chicks) # How man rows in this data set
ncol(chicks) # How many collumns in this data set
mean(chicks$weight) # Calculating mean weight for the chickens

# Calculating the mean for weights for day 0
chicks %>% 
group_by(Diet) %>%  # Use group by function to group the data set by data
filter(Time == 0) %>% 
summarise(Mean_weight = mean(weight)) # Summarise function to calculate the mean

# Chapter 3: Descriptive statistics:
chicks %>% 
  summarise(length = n()) # This command tells us how many rows (Just a longer way of doing it)
length(chicks$weight) # Command to show number of rows?

# Use "c" combine function to create your own vector
nums <- c(13, 66, 17, 776, 35, 13) # Using the concatinate function to assign your own vectors
(mean_num <-sum(nums) / length(nums)) # Long way around to sumarise the mean
nums %>% 
  summarise(mean_nums = mean(nums)) # QUick way to calc mean

nums <- c(13, 66, 17, 776, 35, NA, 13) # Using the concatinate function to assign your own vectors
(mean_num <-sum(nums) / length(nums)) # Long way around to sumarise the mean
mean(na.omit) # Complete this
nums %>% 
  summarise(mean_nums = mean(nums)) # QUick way to calc mean


chicks %>% 
  summarise(mean_wt = mean(weight)) # Calculating mean weight

# Practice (find out what round function does!)
round(mean(chicks$weight), 1) 

# Playing around with the mean
chicks %>% 
  summarise(med_wt = median(weight)) # Calculating the median # Useful for when there are large outliers

nums2 <- c(5, 2, 6, 1000, 1)
mean(nums2) # Calculating the mean 
median (nums2) # Calculating the median

nums3 <- c(5, 2, 6, 13, 1, 13)
median(nums3)
mean(nums3)

# Skewness (Coming back to this)

#3.3 Measures of variation and spread

chicks %>% 
  summarise(sd_wt = sd(weight)) # Calculating the standard variation

# Quantiles
quantile(chicks$weight) # Calculating the quantiles

chicks %>% 
  summarise(min_wt = min(weight), # Use the quantile function to break up the quartiles
            qrt1_wt = quantile(weight, p = 0.25), # calculating the first quartile
            med_wt = median(weight), # Second quartile (which is also the mean)
            qrt3_wt = quantile(weight, p = 0.75), # Thid quarter of data
            max_wt = max(weight)) # These things are very sensitive to outliers
  
  
# Calculating the range of weights
range(chicks$weight)
min(chicks$weight) # Calculating the min weight
max(chicks$weight) # Calculating the max weight


# Exercise
summary(chicks$weight)
Task_1 <- chicks %>%
  filter(Time < 10) %>% 
  group_by(Diet, Time) %>% 
  summarise(min_wt = min(weight), # Use the quantile function to break up the quartiles
            qrt1_wt = quantile(weight, p = 0.25), # calculating the first quartile
            med_wt = median(weight), # Second quartile (which is also the mean)
            qrt3_wt = quantile(weight, p = 0.75), # Thid quarter of data
            max_wt = max(weight)) # These things are very sensitive to outliers

# 09 April 2019
# Biostats Day 1
# Afternoon session ( 14:00-17:00)
# Author: Nicole Okkers

# Load Libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)

# the numerical summary produced by a piped series of functions;
# used for (A), (B) and (C), below
# Need to tidy up the data in order to create graphs
iris.cnt <- iris %>%
  count(Species) %>% # automagically creates a column, n, with the counts
  mutate(prop = n / sum(n)) # creates the relative proportion of each species # Creates a new colums. Divides by the sum of the count
iris.cnt

# Create a ggplot graph
# a stacked bar graph with the cumulative sum of observations
plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) + # Assing x and y values # Do not have to specify anything for x for a bar graph
  geom_bar(width = 1, stat = "identity") + # Tells us to draw the bar with a certain widt of 1. Stat identity plots the height of the bar thats present in the collumn
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

# a stacked bar graph with the relative proportions of observations
# Add Notes to this, understand the code and add notes
plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

# a basic pie chart
plt3 <- plt1 + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()

# Modified pie chart
plt3.1 <- plt2 + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "yellow1") +
  theme_minimal()

# if you seriously want a pie chart, rather use the base R function, `pie()`

# here now a bar graph...
# the default mapping of `geom_bar` is `stat = count`, which is a
# bar for each fo the categories (`Species`), with `count` along y
# DON'T FORGET TO ADD NOTES WHEN YOU GET HOME
plt4 <- ggplot(data = iris, aes(x = Species, fill = Species)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

# Representing all graphs at the same time
ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")
ggarrange(plt1, plt2, plt3, plt3.1, plt4, nrow = 2, ncol = 2, labels = "AUTO")

# Continuous data: 
# a normal frequency histogram, with count along y
# Plotting a histogram
hist1 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(colour = "black", fill = "salmon", alpha = 1) + # Alpha is transparency
  labs(title = "Old Faithful data",
       subtitle = "A vanilla frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean() # Each 30 second interval. How many eruptions happen every 30s?
# This histogram shows a bimodal dist. Either eruptions are very short or very long
# Explore faithful data
summary(faithful)

# when the binwidth is 1, the density histogram *is* the relative
# frequency histogram
hist2 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = ..density..), # Use the default density
                 position = 'identity', binwidth = 1, # Change the binwidth, playing arounf with it
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()

# Exploring data
nrow(faithful)
# if binwidth is something other than 1, the relative frequency in
# a histogram is ..density.. * binwidth
hist3 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = 0.5 * ..density..), # y = half a minute. 30 seconds. adjusting the width of the various different bins. 
                 position = 'identity', binwidth = 0.5,
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)", # adding lables to the graph
       y = "Relative contribution") + theme_pubclean()

# ECDF
# Proportonal and cummalative representation of the proportional occurence of erruptions of time
hist4 <- ggplot(data = faithful, aes(x = eruptions)) + 
  stat_ecdf() +
  labs(title = "Old Faithful data",
       subtitle = "ECDF",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()

# Merg the graphs
ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, labels = "AUTO")


# Iris data histograms
# intervals of petal width (for example)

# first we make long data
iris.long <- iris %>%  # Turn y table into long table
  gather(key = "variable", value = "size", -Species) # You need to creat long data in order to create graph (Is this like tidying up the data?)

ggplot(data = iris.long, aes(x = size)) +
  geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
                 colour = NA, bins = 20,
                 aes(fill = Species)) +
  facet_wrap(~variable) +
  labs(title = "Iris data",
       subtitle = "Grouped frequency histogram",
       x = "Size (mm)",
       y = "Count") +
  theme_pubclean()

# Boxplots
#used to represent continous data and summarise all of those things (IQR, Median, Mean etc)
summary(iris) # Box and wisker plot useful way to graphicall represent differences between variables
plt1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(show.legend = FALSE, notch = FALSE) + theme_pubclean() + # do not need a legend beacuse it becomes redundant
  labs(y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic")) # add comments nicole

plt2 <- ggplot(data = iris.long, aes(x = Species, y = size)) +
  geom_boxplot(fill = "red", alpha = 0.4, notch = FALSE) + # geom_boxplot specifies what graph ou want
  geom_jitter(width = 0.1, shape = 21, colour = "blue", fill = NA, alpha = 0.2) +
  facet_wrap(~variable, nrow = 1) +
  labs(y = "Size (mm)") + theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))

# pair wise scatter plots
# You need to understand this well, spend some time on it
plt1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +
  theme(legend.position = c(0.18, 0.85)) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() + 
  theme_pubclean()

# it is very easy to colour code things in ggplot

plt2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") + 
  theme_pubclean()

# Bar Graphs. Shows mean and shows the spread around the meane. Similar to box and whisker, it doesnt show IQR etc. It rather shows the mean (height of the bar) whiskers above. standard deviation

# first make nice labels for the facets because the default ones
# in the dataframe are not so nice; use the `labeller()` function
# to receive the new variable names defined here
facet.names <- c(Petal.Length = "Petal length",
                 Petal.Width = "Petal width",
                 Sepal.Length = "Sepal length",
                 Sepal.Width = "Sepal width")

# start with the `iris.long` long data that were produced above
# we create summaries of mean and SD and squirt it directly
# into the ggplot functions
iris.long %>% 
  group_by(Species, variable) %>% # 
  summarise(mean.size = mean(size),
            sd.size = sd(size)) %>%
  ggplot(aes(x = Species, y = mean.size)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean.size - sd.size, ymax = mean.size + sd.size), width = 0.2) +
  facet_wrap(~variable, labeller = labeller(variable = facet.names)) +
  labs(y = "Size (mm)", title = "A box plot...", subtitle = "...of the Iris data") +
  theme(axis.text.x = element_text(face = "italic"))


# This script is very neat
# Shows clear understanding of the content and bits of code
# With your stats assignment first explore your data, then do the normaility tests and then after that you can decide which statistical tests you should run on the data
# So it is good to explore the data first as you have done in the first few lines of the script
# You were not in class when we did the normality and correlation tests
# So please test for normailty and then decide which statistical tests and graphs to choose
