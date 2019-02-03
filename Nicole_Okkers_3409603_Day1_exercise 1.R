# Day_1. RN
# calculate a monthly climatology per site
# Author: N Okkers
# Date: 29 January 2019

library(tidyverse)
library(lubridate)

temp <- read_csv("data/SACTN_data.csv")


temp2 <- temp %>%
  dplyr::mutate(month = month(date)) %>% 
  dplyr::group_by(site, month) %>% 
  dplyr::summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  ungroup()

# Neat
# Lack of comments
  