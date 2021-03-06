# Plotting in R using ggplot
# Day 2
# Nicole Okkers
# 30 January 2019

# Load Libraries
library(tidyverse)

Chicks <- datasets::ChickWeight # this is the command to 'activate' the dataset 'chickweight' (for the exercise, choose 3 data sets from this built-in data sets)
??ChickWeight

ggplot(data = Chicks, aes( x = Time, y = weight)) +
  geom_point() +
  geom_line(aes(group = Chick))

ggplot(Chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))
 
ggplot(Chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(Chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point(color = "green") + # assinging colours to the points
  geom_line(aes(group = Chick))

ggplot(Chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point(aes (size = weight )) +
  geom_smooth(method = "lm") +
labs(x = "Days", y ="weight (kg)") + # Changing Lables
  ggtitle( "A") +
  theme_bw()

# Facetting in ggplot
library(ggpubr) # run the ggpubr library in order to activate "facet_wrap" and "ggarrange"

ggplot(Chicks, aes( x = Time, y = weight , colour = Diet)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~Diet, ncol = 4)

Chicks_2 <- Chicks %>% 
  filter(Time == 21)

plot_1 <- ggplot(Chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y ='weight (kg)') +
  ggtitle( "Chick data: Days vs Weight")
plot_1

plot_2 <- ggplot(Chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("B")
plot_2

plot3 <- ggplot(data = Chicks_2, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) + # "position" makes sure that the bars are next(alongside eachother) to eachother and not on top or behind eachother
  labs(x = "Final Mass (g)", y ="Count") # instruction to make histogram type graph
plot3

plot4 <- ggplot(data = Chicks_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) + # this is the command to produce a boxplot
  labs(x = "Diet", y = "Final Mass (g)")
plot4

plot_combined <- ggarrange(plot_1, plot_2, plot3, plot4) # install package ggpubr and then use the ggarrange to combine all of the plots

# third Library
library(boot)

Urine <- boot::urine # Assigning a name to the data set so that it shows up in the environment
??Urine # use the help function to understand the data a bit more

Urine %>%
  select(-cond) # This is the command to remove a collumn ; in this case, the conductivity collumn was removed

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) # command to create a scatter plot graph. The scale (colour variation) helps you to compare

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))
N

# [A.A]
# Neat script
# SCript runs complete
# Sufficeint comments, nicely done
