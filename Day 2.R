# 15 April 2019
# Day 2 Biostats
# Moring Session (9:00 am - 12 noon)
# Chapter 5: Distrubutions
# Author: Nicole Okkers

library(fitdistrplus)
library(logspline)
# Generate log-normal data
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

# Normally distributed data
par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)
# Notes to help you understand what is going on
# Also add your notes before you push to git hub
## summary statistics
## ------
## min:  16.62   max:  54.15 
## median:  40.38 
## mean:  40.28434 
## estimated sd:  7.420034 
## estimated skewness:  -0.551717 
## estimated kurtosis:  3.56516

#Normally Distributed
y_1 <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742)
par(mfrow = c(1, 1))
plot(x = c(1:length(y_1)), y = y)
hist(y_1)
descdist(y_1, discrete = FALSE, boot = 100)

#generate random data
y <- rnorm(100, 13, 2)# Normal distribution
par(mfrow = c(2, 2)) #dev.off() to get rid of par command
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

# Random normal data


# Create histogram


y <- rnorm(10, 13, 2)# Normal distribution
length(y)
mean(y)
sd(y)
hist(y)
descdist(y, discrete = FALSE) #describing distribution


# Uniformly distributed data

# Random data
set.seed(666)
# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))
dim(r_dat) # Checking out the dimensions
head(r_dat)
tail(r_dat)

# Create histogram
library(ggplot2)
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

# Shaipiro test
shapiro.test(r_dat$dat) # This data is not normally distributed # Dollar sign is choosing the particular collumn dat

library(tidyverse)
#Proper shapiro test
# we use the square bracket notation to select only the p-value;
# had we used `[1]` we'd have gotten W
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric (shapiro.test(dat)[2])) # looks for second value(p) and puts the P value in the table.So the vaulues you see are the p-values

str(r_dat) # To see variables in data set
r_dat$sample # Showing the internal set up of the data set?

str(h)

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

# One sample t-tests
# create a single sample of random normal data
library(ggpubr)
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

# One sample t-test
# No variance to compare
# ...

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 15) # Disproving alternative

# Plotting box plot for the one-sample t-test
ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

# One-sided one sample t-tests
# check against the trailing tail
t.test(r_one$dat, mu = 30, alternative = "less") # Are the data in our sample less than 30?

# check against the leading tail
t.test(r_one$dat, mu = 30, alternative = "greater") # Are the data in our sample greater than 30?

# Two sample tests
# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE) # T-test function # ARe the data different between the two levels of sample
# if the variances are not equal, simply set `var.equal` to false
# and a Welch's t-test will be performed

# 15 April 2019
# Day 2 Biostats
# Afternoon Session (14:00 - 17:00)
# Chapter 7: ANOVA

# First grab the data
# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# 2 sample independent 2 tailed t-rest

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
t.test(weight ~ Diet, data = chicks_sub) # Command for T test

# ANOVA is used for more than 2 goups/populations 
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21)) # Use filter function to select what you wanna run an ANOVA for 
summary(chicks.aov1) # one of trhe four diff diets resulted in a higher mass for the chickens. Diets differ in the ability to influence the mass at 21 days

PLOT_1 <- ggplot(data = chicks.aov1  , aes(x = Diet, y = weight)) + # Attempt at graphically representing the T-test
  geom_boxplot(aes(fill = Diet),notch = TRUE) +
  labs(y = "Weight (Kg)") + theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) +
PLOT_1

TukeyHSD(chicks.aov1, ordered = TRUE)
plot(TukeyHSD(chicks.aov1, ordered = TRUE)) # Plotting the Tukey test

#if asked but were they the same weight at start? filter for day 1
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))


summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2)))) # at day 2
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(10)))) # at day 10

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(21))))


#asking all question for a specific time
chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 21)))
summary(chicks.aov2)

#real one
summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

# Interaction between diet and time
summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(4, 21))))
summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(4, 21))))        

TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(20, 21))))

