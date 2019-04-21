# 16 April 2019
# Biostats Day 3
# Author: Nicole Okkers
# Chapter 8 : Simple Linear regressions


#Load libraries
library(tidyverse)
library(ggplot2)

# Explore datac set
head(faithful)

#Linear Model
#eruptions as a function of time // of the dataset 'faithful'
eruption.lm <- lm(eruptions ~ waiting, data = faithful) #naming the linear model
summary(eruption.lm) #summary of linear model

str(eruption.lm)

#null hypothesis is rejected
#there is a relationship between waiting time and eruption time


# 8.1.3 A Graph of Linear regression
#x-axis: waiting
#y-axis: eruption
#properly labelled

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

# Predicting from the Linear Model
# use the accessor function to grab the coefficients:
erupt.coef <- coefficients(eruption.lm)
erupt.coef
# how long would an eruption last of we waited, say, 80 minutes?
waiting <- 80 

# the first and second coef. can be accessed using the 
# square bracket notation:
erupt.pred <- erupt.coef[1] + (erupt.coef[2] * waiting)
erupt.pred # the unit is minutes

pred.val <- data.frame(waiting = c(60, 80, 100))
predict(eruption.lm, pred.val) # returns waiting time in minutes

# The co efficient of determination
summary(eruption.lm)$r.squared

library(tidyverse)
n <- 100
set.seed(666)
rand.df <- data.frame(x = seq(1:n),
                      y = rnorm(n = n, mean = 20, sd = 3))
ggplot(data = rand.df, aes(x = x, y = y)) +
  geom_point(colour = "blue") +
  stat_smooth(method = "lm", colour = "purple", size = 0.75, fill = "turquoise", alpha = 0.3) +
  labs(title = "Random normal data",
       subtitle = "Linear regression",
       x = "X (independent variable)",
       y = "Y (dependent variable)")

# Neat scripts
# Good comments
# Maybe while you are working on your scripts you can add a bit of the discriptions when AJ discusses or interprets the findings
# This may not be as important now but for quantitative ecology most of your marks will be gained by interpretations
# So a question worth 10 marks: 8marks interpretations and 2marks code
