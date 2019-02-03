# Day 1
# Laminaria dataset exploring and learning
# Nicole Okkers
# 29 January 2019

# Loading Libraries

library(tidyverse)
library(lubridate)
Lam <- read_csv("data/laminaria.csv") # Read the CSV file for laminaria
head(Lam) # shows first 6 rows
tail(Lam) # shows last 6 rows
head(Lam, n = 3) # Shows First 3 rows
tail(Lam, n = 3) # Shows Last 3 rows

Lam_select <- Lam %>% # exploring data by selecting certain rows
  select(site, total_length) %>% # only selected "site" and total_length rows 
  slice(54:80)

Lam_kom <- Lam %>% 
  filter(site == "Kommetjie") # to see Kommetjie data/ Practising with "filter" function

Lam_try <- Lam %>% 
  select(site, blade_length) %>% 
  filter(site == "Sea Point")

Lam %>% 
  filter(total_length == max(total_length))

summary(Lam)

Lam %>% 
  summarise(avrg_bl = mean(blade_length)) #summarise function is used to calculate stuff like: mean, median etc
library(tidyverse)
Lam %>% 
  summarise(avrg_bl = mean(blade_length),
            med_bl = median(blade_length),
            sd_bl = sd(blade_length)) # use namees that are easy to remember ie:bl=Blade_lenght

Lam %>% 
  group_by(site) %>% 
  summarise(var_bl = var(blade_length),
            n = n()) %>% 
  mutate(se = sqrt(var_bl/n))

Lam_2 <- Lam %>%  # this is how to drop from 12 variables to 10 variables and to show you how to exclude different columns
  select (-blade_thickness, -blade_length)

Lam_count <- Lam %>% # This will tell us how many specimens were sampled for stipe_mass
  select(stipe_mass) %>% 
  summarise(n = n())

Lam %>%  # how to emit data that is listed as "NA"
  select(stipe_mass) %>%
  na.omit %>% 
  summarise(n = n())

Lam %>%  
  select(blade_length) %>%
  summarise(n = n())

Lam %>%  
  select(blade_length) %>%
  na.omit %>% 
  summarise(n = n())

ggplot(data = Lam, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "Green", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")


  
Lam %>% 
  group_by(site) %>% 
  summarise(mean_blade_length = mean(blade_length),
            min_blade_length = min(blade_length),
            max_blade_lenght = max(blade_length),
            n = n())

Lam %>%   
  group_by(site) %>% 
  filter(stipe_mass == max(stipe_mass)) %>% 
  select(site, region, stipe_length) %>% 
  arrange(site)

Lam_3 <- Lam %>% 
  mutate(total_length_half = (total_length/2)) %>% 
  na.omit %>% 
  filter(total_length_half>100) %>% 
  select(site, total_length_half) 

library(tidyverse)
Lam_3 <- Lam %>% 
  mutate(total_length_half = (total_length/2)) %>%  # answer to question 1
  na.omit %>% 
  filter(total_length_half>100) %>% 
  select(site, total_length_half) 

#[A.A]
# script runs complete
# Nice and neat script
# Great form of commmenting
# Could add some comments in the larger chuncks on code for future reference 
# Nicely done
