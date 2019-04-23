# 07 February 2019
# Author ; Nicole Okkers
# Intro to R workshop: Yearly Egg Breadths exercise


# Load Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(dplyr)

# Load Data
library(readr)
YEB_1 <- read_delim("data/YearlyEggBreadths.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Command to load the data. The file was not in a "table fomr" this is the code that allows R to read the data properly

YEB_2_tidy <- YEB_1 %>% 
  slice(1:12) %>% 
  select(AveragesMinBreadth1, StdDevMinBreadth1, AveragesMinBreadth2, StdDevMinBreadth2) %>% 
  mutate(Months_redone = c(1:12))

  grDevices::colours() # Accessing the different colours that R has 

Attempt_1 <- ggplot(YEB_2_tidy, aes(x = Months_redone)) + 
  geom_line(aes(y = AveragesMinBreadth1), colour = "black" ) + 
  geom_line(aes(y = AveragesMinBreadth2), colour = "orange" ) +
  scale_x_continuous(breaks = seq(1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12), labels = c("Jan" , "Feb" , "March" , "April" , "May" , "June" , "July" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec")) +
  scale_y_continuous(breaks = seq(10 , 20 , 30 , 40 , 50 , 60 , 70), labels = c("10" , "20" , "30" , "40" , "50" , "60" , "70")) +
  geom_errorbar(aes(ymin = AveragesMinBreadth1 - StdDevMinBreadth1, # Error bars added using command geom_errorbar
                    ymax = AveragesMaxBreadth2 + StdDevMinBreadth2), # setting ymax and ymin
                position = "dodge")
  
Attempt_1


