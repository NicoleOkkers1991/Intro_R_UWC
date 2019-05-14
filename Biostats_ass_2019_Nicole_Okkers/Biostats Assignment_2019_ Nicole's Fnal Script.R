# 03 May 2019
# Biostats Assignment 2019 : R Script for my Statistical Analysis
# Due Date: 13 May 2019
# Author : Nicole Okkers

# Load Libraries: 
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)
library(reshape2)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(plotly)
library(e1071)
library(ggplot2)
library(ggsignif)

# Load CSV File into R
# This is not a built-in data set
Codylid <- Codylid_Data <- read_csv("data/Codylid_Data.csv") # Command to Load data set into R and assigng it a name. 

# View/ Explore Data
Codylid_exp <- as_tibble(Codylid) # assigning it as a table to sede the data 
head(Codylid, n = 9) # first 9 rows
tail(Codylid, n = 9) # Last 9 rows 
colnames(Codylid) # listing of variables names
summary(Codylid) # mean, median, mode etc
dim(Codylid) # exploring dimensions of the data set
glimpse(Codylid) # Glimpse helps you list the variables in your data frame # Listing varibales in the rast_feb data frame
ncol(Codylid) # How many collumns in the data set
summarise(Codylid) # Command to summarise the data


# Exploring and understanding the data set further
Codylid %>% 
  summarise(length = n()) # To determine the size of the sample space

# Measures of Central Tendancy 
# Means
Codylid %>% 
  summarise(mean_SVL = mean(SVL)) # Mean for the SVL Collumn
Codylid %>% 
  summarise(mean_HV = mean(`Head Volume`)) # Mean for the Head Volume Collumn
Codylid %>% 
  summarise(mean_HL = mean(`Head Length`)) # Mean for the Head Lenght Collumn
Codylid %>% 
  summarise(mean_HH = mean(`Head Height`)) # Mean for the Head Height Collumn

# Median
Codylid %>%
  summarise(med_SVL = median(SVL)) # Median for SVL collumn
Codylid %>%
  summarise(med_HV = median(`Head Volume`)) # Median for Head Volume collumn
Codylid %>%
  summarise(med_HL = median(`Head Length`)) # Median for Head Lenght collumn
Codylid %>%
  summarise(med_HH = median(`Head Height`)) # Median for Head Height collumn

# These means do not equal the median. so this is not normally distributed data?

# Skewness
skewness(Codylidt$SVL)  # Skewness for SVL # Negatively skewed  = left skewed distribution
skewness(Codylid$`Head Volume`)  # Skewness for Head Volume # Positively skewed =  Right- skewed distribution
skewness(Codylid$`Head Length`)  # Skewness for Head Lenght # Negatively skewed  = left skewed distribution
skewness(Codylid$`Head Height`)  # Skewness for Head Height # Positively skewed =  Right- skewed distribution

# Negative skewness = mean of the data is less than their median.
# Positive skewness = Mean of the data is larger than their median.

# Kurtosis
# Describes the tail shape of the data's distribution
# Normal Distribution has zero Kurtosis (Standard tail shape)
# Negative Kurtosis = data with a thin-tailed distribution
# Positive Kurtosis = data with a fat-tailed distribution 
kurtosis(Codylid$SVL) # Kurtosis for SVL Collumn # Negative Kurtosis = Thin-tail distribution
kurtosis(Codylid$`Head Volume`) # Kurtosis for Head Volume Collumn # Positive Kurtosis = Fat-tail distribution
kurtosis(Codylid$`Head Length`) # Kurtosis for Head Length Collumn # Negative Kurtosis = Thin-tailed distribution
kurtosis(Codylid$`Head Height`) # Kurtosis for Head Height Collumn # Posiive Kurtosis = Fat-tailed distribution

# Measures of Spread around the central/mean value
# Calculating standard deviation
# the smaller the standard deviation, the closer the sample data are to the mean # This only seems true for the HL and HH collumns
Codylid%>%
  summarise(sd_SVL = sd(SVL)) # Standard devaition for SVL collumn
Codylid %>%
  summarise(sd_HV = sd(`Head Volume`)) # Standard devaition for Head Volume collumn
Codylid %>%
  summarise(sd_HL = sd(`Head Length`)) # Standard devaition for Head Lenght collumn
Codylid %>%
  summarise(sd_HH = sd(`Head Height`)) # Standard devaition for Head Height collumn
# This data doesn't seem to be normally distributed, so will have to find appropriate ways to express the spread of the data

# Calculating Quantiles
# This is useful if the dataset consists of outliers
quantile(Codylid$SVL) # Quantile for SVL Collumn
quantile(Codylid$`Head Volume`) # Quantile for Head Volume Collumn
quantile(Codylid$`Head Length`) # Quantile for Head Length Collumn
quantile(Codylid$`Head Height`) # Quantile for Head Height Collumn

# Another way to determine quartiles for SVL Collumn
Codylid %>% 
  summarise(min_SVL = min(SVL),
          qrt1_SVL = quantile(SVL, p = 0.25),
          med_SVL = median(SVL),
          qrt3_SVL = median(SVL, p = 0.75),
          max_SVL = max(SVL))

# Another way to determine quartiles for Head Volume Collumn
Codylid %>% 
  summarise(min_HV = min(`Head Volume`),
            qrt1_HV = quantile(`Head Volume`, p = 0.25),
            med_HV = median(`Head Volume`),
            qrt3_HV = median(`Head Volume`, p = 0.75),
            max_HV = max(`Head Volume`))

# Another way to determine quartiles for Head Lenght Collumn
Codylid %>% 
  summarise(min_HL = min(`Head Length`),
            qrt1_HL = quantile(`Head Length`, p = 0.25),
            med_HL = median(`Head Length`),
            qrt3_HL = median(`Head Length`, p = 0.75),
            max_HL = max(`Head Length`))

# Another way to determine quartiles for Head Height Collumn
Codylid %>% 
  summarise(min_HH = min(`Head Height`),
            qrt1_HH = quantile(`Head Height`, p = 0.25),
            med_HH = median(`Head Height`),
            qrt3_HH = median(`Head Height`, p = 0.75),
            max_HH = max(`Head Height`))

# Minimum, Maximum and Range
range(Codylid$SVL) # Range for SVL Collumn
range(Codylid$`Head Volume`) # Range for Head Volume Collumn
range(Codylid$`Head Length`) # Range for Head Length Collumn
range(Codylid$`Head Height`) # Range for Head Height Collumn
# This command only gives the min and max value, we need to calculate the range: 
# Calculating the Range::
range(Codylid$SVL)[2] - range(Codylid$SVL)[1] # Range for SVL collumn
range(Codylid$`Head Volume`)[2] - range(Codylid$`Head Volume`)[1] # Range for Head Volume collumn
range(Codylid$`Head Length`)[2] - range(Codylid$`Head Length`)[1] # Range for Head Length collumn
range(Codylid$`Head Height`)[2] - range(Codylid$`Head Height`)[1] # Range for Head Height collumn

# Groupwise summary statistics
# I am doing this so I can see if there are patterns accross the three different species
# Hopefully this will help me forumalate a hypthesis 

# How are my data distributed? 

# Normally distributed data
descdist(Codylid$SVL, discrete = FALSE, boot = 100) # Cullen Frey to check for Normal Distribution for SVL # Normally Distributed
descdist(Codylid$`Head Volume`, discrete = FALSE, boot = 100) # Cullen Frey to check for Normal Distribution for Head Volume # Normally distributed
descdist(Codylid$`Head Length`, discrete = FALSE, boot = 100)# Cullen Frey to check for Normal Distribution for Head Length # Normally distributed
descdist(Codylid$`Head Height`, discrete = FALSE, boot = 100)# Cullen Frey to check for Normal Distribution for Head Height # Normally Distributed

# Cullen Frey Shows that All Data is Normally distributed

# Inferences
# Determine Normailty
# Shapiro-Wilk Test:
Codylid %>% # Shapiro Wilk test for SVL
  group_by(Species) %>% # Data Seems to be normally distributed
  summarise(norm_SVL = as.numeric(shapiro.test(SVL)[2]))

## Shapiro-Wilk Test:
Codylid %>% # Shapiro Wilk test for Head Volume
  group_by(Species) %>% # Data Seems to be normally distributed
  summarise(norm_HV = as.numeric(shapiro.test(`Head Volume`)[2]))

# Shapiro-Wilk Test:
Codylid %>% # Shapiro Wilk test for Head Length
  group_by(Species) %>% # Data Seems to be normally distributed
  summarise(norm_HL = as.numeric(shapiro.test(`Head Length`)[2]))

# Shapiro-Wilk Test:
Codylid %>% # Shapiro Wilk test for Head Height
  group_by(Species) %>% # Data Seems to be normally distributed
  summarise(norm_HH = as.numeric(shapiro.test(`Head Height`)[2]))

# Checking Homoscedacity for SVL
Codylid %>% 
  group_by(Species) %>% 
  summarise(Species_var_SVL = var(SVL)) # Variance of one is not more than two to four times greater than the other

# Checking Homoscedacity for Head Volume
Codylid %>% 
  group_by(Species) %>% 
  summarise(Species_var_HV = var(`Head Volume`)) # This is not Homoscedastic # use function t.test

# Checking Homoscedacity for Head Height
Codylid %>% 
  group_by(Species) %>% 
  summarise(Species_var_HH = var(`Head Height`)) # Variance of one is not more than two to four times greater than the other

# Checking Homoscedacity for Head height
Codylid %>% 
  group_by(Species) %>% 
  summarise(Species_var = var(SVL)) # Variance of one is not more than two to four times greater than the other

# Checking Homoscedacity for Head Length
Codylid %>% 
  group_by(Species) %>% 
  summarise(Species_var_HL = var(`Head Length`)) # Variance of one is not more than two to four times greater than the other

# The RAW data was Log transformed Using the Following Commands: 
# 
## Name_2 <- data.frame(log10(Snout$`Head Volume`))  
## Name_3 <- data.frame(log10(Snout$`Head Length`))  
## Name_4 <- data.frame(sqrt(Snout$`Head Height`))
## Name_5 <- data.frame(Snout$Species)

# The Data Is Normally Distributed, I will Now start the Statistical Analysis of the data:

# ANOVA
# To test for inter- and intraspecific variation in head morphology, a one-way analysis of variance (ANOVA) was used
# Hypothesis for ANOVA: 
# H0 : There is no differences in means: The head morphologies (Head Volume) do not differ accross the three species of Cordylids
# H1 : There is a difference in means: The Head Morphologies (Head Volume) differ accross the three species of Cordylids
Codylid.aov1 <- aov(`Head Volume` ~ Species, data = Codylid)
summary(Codylid.aov1) # P < 0.05 
# H0 is rejected and the alternative hypothesis is accepted
# Tukey Test
# Create a set of confidence intervals on the differences between the means of the 
#levels of a factor with the specified family-wise probability of coverage. 
#The intervals are based on the Studentized range statistic, Tukey's ‘Honest 
#Significant Difference’ method.Usage
Tukey_1 <- TukeyHSD( x = Codylid.aov1)
# Visualise ANOVA
boxplt1_HV <- ggplot(data = Codylid.aov1, aes(x = Species, y = `Head Volume` , fill = Species)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE) + theme_classic2() +
  theme(axis.text.x = element_text(face = "italic")) + #species name in italics
  labs(title = "Intra and Interspecific variation of Head volume for Three Closely related Cordylid Species",
       subtitle = "ANOVA Visualised as Boxplot",
       x = "Species",
       y = "Head Volume (mm)") 
boxplt1_HV
boxplt1_HV_Final <- boxplt1_HV+scale_color_brewer(palette="Dark2")

# Tuckey test representation :
Tukey_HV <- plot(Tukey_1 , las=1 , col="brown" )  # Come back after submission to figure out how to properly visually represent Tukey test

# To test for inter- and intraspecific variation in head morphology, a one-way analysis of variance (ANOVA) was used
# Hypothesis for ANOVA: 
# H0 : There is no differences in means: The head morphologies (Head_length) do not differ accross the three species of Cordylids
# H1 : There is a difference in means: The Head Morphologies (head Length) differ accross the three species of Cordylids
Codylid.aov2 <- aov(`Head Length` ~ Species, data = Codylid)
summary(Codylid.aov2) # P < 0.05 
# H0 is rejected and the alternative hypothesis is accepted
# Tukey Test
# Create a set of confidence intervals on the differences between the means of the 
#levels of a factor with the specified family-wise probability of coverage. 
#The intervals are based on the Studentized range statistic, Tukey's ‘Honest 
#Significant Difference’ method.Usage
Tukey_2 <- TukeyHSD(Codylid.aov2)
# Visualise ANOVA
boxplt2_HL <- ggplot(data = Codylid.aov2, aes(x = Species, y = `Head Length` , fill = Species)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE) + theme_classic2() +
  theme(axis.text.x = element_text(face = "italic")) + #species name in italics
  labs(title = "Intra and Interspecific variation of Head Length for Three Closely related Cordylid Species",
       subtitle = "ANOVA Visualised as Boxplot",
       x = "Species",
       y = "Head Length (mm)") 
boxplt2_HL
boxplt2_HL_Final <- boxplt2_HL+scale_color_brewer(palette="Dark2")

# Tuckey test representation :
Tukey_HL <- plot(Tukey_2 , las=1 , col="brown" )  # Come back after submission to figure out how to properly visually represent Tukey test

# To test for inter- and intraspecific variation in head morphology, a one-way analysis of variance (ANOVA) was used
# Hypothesis for ANOVA: 
# H0 : There is no differences hin means: The head morphologies (Head_length) do not differ accross the three species of Cordylids
# H1 : There is a difference in means: The Head Morphologies (head Length) differ accross the three species of Cordylids
Codylid.aov3 <- aov(`Head Height` ~ Species, data = Codylid)
summary(Codylid.aov3) # P < 0.05 
# H0 is rejected and the alternative hypothesis is accepted
# Tukey Test
# Create a set of confidence intervals on the differences between the means of the 
#levels of a factor with the specified family-wise probability of coverage. 
#The intervals are based on the Studentized range statistic, Tukey's ‘Honest 
#Significant Difference’ method.Usage
Tukey_3 <- TukeyHSD(Codylid.aov3)
# Visualise ANOVA
boxplt3_HH <- ggplot(data = Codylid.aov3, aes(x = Species, y = `Head Height` , fill = Species)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE) + theme_classic2() + # Command for theme 
  theme(axis.text.x = element_text(face = "italic")) + #species name in italics
  labs(title = "Intra and Interspecific variation of Head Height for Three Closely related Cordylid Species",
       subtitle = "ANOVA Visualised as Boxplot",
       x = "Species",
       y = "Head Height (mm)") 
boxplt3_HH
boxplt3_HH_Final <- boxplt3_HH+scale_color_brewer(palette="Dark2")

# Tuckey test representation :
Tukey_HH <- plot(Tukey_3 , las=1 , col="brown" )  # Come back after submission to figure out how to properly visually represent Tukey test
ggarrange(boxplt1_HV,boxplt2_HL,boxplt3_HH, ncol = 2, nrow = 2, labels = "AUTO") # Graph to put in Final Write-up

# I will use Correlation to investigate potential relationships between variables from the same sample

Codylid.aov1_HV <- ggplot(Codylid.aov1, aes(x = group, y = scores)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") +
  ylab("Scores") +
  geom_signif(comparisons = list(c("group_1", "group_2")), 
              map_signif_level=TRUE) # COME BACK TO THIS IN THE AM
Codylid.aov1_HV

Codylid.aov1_HV_1 <- ggplot(data = Codylid.aov1, aes(x = Species, y = `Head Volume` )) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = size = 2.0, linetype = "solid", show.legend = F) +
                 geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
                 geom_jitter(width = 0.05))


#Pearson correlation
#Specify which correlation is done
# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# Correlation for SVL vs Head Volume
cor.test(x = Codylid$SVL, Codylid$`Head Volume`, #this is the code always used
         use = "everything", method = "pearson") #can be changed but not normally done
#do a correlation test on the ecklonia dataset on the stipe length column
#and then compare it with the frond length 

#can write out the results in the same format, just type properly 

#a great correlation is closer to 1 

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", #outcomes in environment as a value
                  round(cor(x = Codylid$SVL, Codylid$`Head Volume`),2))
# Strong Cor for HV, wiith the cor value = 0.83

# Then create a single panel showing one correlation
ggplot(data = Codylid, aes(x = SVL, y =`Head Volume`)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Pearson Cor for SVL vs Head Height
cor.test(x = Codylid$SVL, Codylid$`Head Height`, #this is the code always used
         use = "everything", method = "pearson") #can be changed but not normally done
#do a correlation test on the ecklonia dataset on the stipe length column
#and then compare it with the frond length 

#can write out the results in the same format, just type properly 

#a great correlation is closer to 1 

# Calculate Pearson r beforehand for plotting
r_print_HH <- paste0("r = ", #outcomes in environment as a value
                  round(cor(x = Codylid$SVL, Codylid$`Head Height`),2))
# Strong Cor for HV, wiith the cor value = 0.11

# Then create a single panel showing one correlation
ggplot(data = Codylid, aes(x = SVL, y =`Head Height`,)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Pearson Cor for SVL vs Head HLength
cor.test(x = Codylid$SVL, Codylid$`Head Length`, #this is the code always used
         use = "everything", method = "pearson") #can be changed but not normally done
#do a correlation test on the ecklonia dataset on the stipe length column
#and then compare it with the frond length 

#can write out the results in the same format, just type properly 

#a great correlation is closer to 1 

# Calculate Pearson r beforehand for plotting
r_print_HH <- paste0("r = ", #outcomes in environment as a value
                     round(cor(x = Codylid$SVL, Codylid$`Head Length`),2))
# Strong Cor for HV, wiith the cor value = -0.08

# Then create a single panel showing one correlation
ggplot(data = Codylid, aes(x = SVL, y =`Head Length`)) + # Command to plot graph
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# The Pearson's correlation was the wrong analysis for this data. 
# A regression analysis is needed because I want to test the statistical significance of the 
# dependence of one continous variable on or many independent continous variables. 
#As a result of the inter and intraspecific differences in body size, all the morphological 
#variables Head Height, Head length and Head volume were regressed against snout-vent length 

#Linear Model to test intra and inter specific variation/ differences
# SVL Versus Head Lenght
# H0 = Head Length is dependent on SVL
# H1 = Head Length is not dependent on SVL
Codylid_reg_HL <- lm(SVL ~`Head Length`, data = Codylid) #naming the linear model
summary(Codylid_reg_HL) #summary of linear model
# The coefficient of determination/ slope? for SVL verse Head Lenght is  -0.12707
# It tells us how much one unit in change of the independent variable determines the 
# corresponding change in the response variable.
#  p-value: 0.3888 therefore P > 0.05 The null hypothesis is accepted
str(Codylid_reg_HL) # Further analysing the regression

# Calculating the Regressio slope manually
slope_HL <- 1.83934 *  -0.12707 # Calculating the Slop by multiplying the Intercept 
# estimate with the Head Lenght estimate. Values taken from the summary of the Regression analysis.
# p.val <- round(coefficients(summary(Codylid_reg_HL))
p.val = 0.3888
r2_HL <- round(summary(Codylid_reg_HL)$r.squared, 3) # 0.006

# Change point shapes, colors and sizes
R_SV_HL_Plot <- ggplot(Codylid, aes(x=SVL, y=`Head Length`, shape=Species, color=Species)) +
  geom_point()+
  annotate("text", x = 2 , y = 3.5, label = paste0("slope == ", slope_HL, "~(mm/mm)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3.25, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3, label = paste0("italic(r)^2 == ", r2_HL), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "aquamarine", se = FALSE, fullrange = TRUE) +
  labs(subtitle = "Linear regression",
       x = "Snout Vent Length (mm)",
       y = "Head Length (mm)") +
  theme_classic()
R_SV_HL_Plot

# Use brewer color palettes
Final_Reg_SVL_HL <- R_SV_HL_Plot+scale_color_brewer(palette="Dark2") # Final Regression Pl

#Linear Model to test intra and inter specific variation/ differences for SVL Versus Head Volume
# SVL Versus Head Volume
# H0 = Head Volume is dependent on SVL
# H1 = Head Volume is not dependent on SVL
Codylid_reg_HV <- lm(SVL ~`Head Volume`, data = Codylid) #naming the linear model
summary(Codylid_reg_HV) #summary of linear model
# The coefficient of determination/ slope? for SVL verse Head Lenght is  -0.12707
# It tells us how much one unit in change of the independent variable determines the 
# corresponding change in the response variable.
# p-value:2.2e-16 Therefore P < 0.05. The null hypothesis is rejected
str(Codylid_reg_HV) # Further analysing the regression

# Calculating the Regressio slope manually
slope_HV <- 1.34780 * 0.32204 # Calculating the Slop by multiplying the Intercept 
# estimate with the Head Lenght estimate. Values taken from the summary of the Regression analysis.
# p.val <- round(coefficients(summary(Codylid_reg_HV))
p.val = 2.2e-16 # P < 0.05
r2_HV <- round(summary(Codylid_reg_HV)$r.squared, 3) # 0.695

# Plotting the regression 
R_SV_HV_Plot <- ggplot(Codylid, aes(x=SVL, y=`Head Volume`, shape=Species, color=Species)) +
  geom_point()+
  annotate("text", x = 1.95 , y = 3.5, label = paste0("slope == ", slope_HV, "~(mm/mm)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3.25, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3, label = paste0("italic(r)^2 == ", r2_HV), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "aquamarine", se = FALSE, fullrange = TRUE) +
  labs(title = "Snout vent Length Verse Head Volume for three closely related Cordylid Species",
       subtitle = "Linear regression",
       x = "Snout Vent Length (mm)",
       y = "Head Volume (mm)") +
  theme_classic()
R_SV_HV_Plot

# Use brewer color palettes
Final_Reg_SVL_HV <- R_SV_HV_Plot+scale_color_brewer(palette="Dark2") # Final Regression Plot for the Head volume and Snout Length


#Linear Model to test intra and inter specific variation/ differences for SVL Versus Head Height
# SVL Versus Head Height
# H0 = Head Height is dependent on SVL
# H1 = Head Height is not dependent on SVL
Codylid_reg_HH <- lm(SVL ~`Head Height`, data = Codylid) #naming the linear model
summary(Codylid_reg_HH) #summary of linear model
# The coefficient of determination/ slope? for SVL verse Head Lenght is  -0.12707
# It tells us how much one unit in change of the independent variable determines the 
# corresponding change in the response variable.
# p-value:2. 0.2084 Therefore P > 0.05. The null hypothesis is accepted
str(Codylid_reg_HH) # Further analysing the regression

# Calculating the Regressio slope manually
slope_HH <- 1.7703 * 0.3981 # Calculating the Slop by multiplying the Intercept 
# estimate with the Head Lenght estimate. Values taken from the summary of the Regression analysis.
# p.val <- round(coefficients(summary(Codylid_reg_HV))
p.val =  0.2084 # P > 0.05
r2_HH <- round(summary(Codylid_reg_HH)$r.squared, 3) # 0.13

# Plotting the regression 
R_SV_HH_Plot <- ggplot(Codylid, aes(x=SVL, y=`Head Height`, shape=Species, color=Species)) +
  geom_point()+
  annotate("text", x = 1.95 , y = 3.5, label = paste0("slope == ", slope_HH, "~(mm/mm)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3.25, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 2 , y = 3, label = paste0("italic(r)^2 == ", r2_HH), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "aquamarine", se = FALSE, fullrange = TRUE) +
  labs(title = "Snout vent Length Verse Head Height for three closely related Cordylid Species",
       subtitle = "Linear regression",
       x = "Snout Vent Height (mm)",
       y = "Head Volume (mm)") +
  theme_classic()
R_SV_HH_Plot

# Use brewer color palettes
Final_Reg_SVL_HH <- R_SV_HV_Plot+scale_color_brewer(palette="Dark2") # Final Regression Plot for the Head Height and Snout Length
