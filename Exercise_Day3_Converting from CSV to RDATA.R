# Day 4
# Exercise 1 : Convert a CSV File to RData file Type
# Nicole Okkers
# 31 January 2019

# Conversion from CSV to RData file type
Lam <- read_csv("data/laminaria.csv") # Read the CSV file for laminaria
save(Lam,file = "data/laminaria.RData") # Converting from CSV to RData


# Day 4
# Exercise 2 : Write a five line paragraph giving a description of library (ggsn) and (scales)
# Nicole Okkers
# 31 January 2019

??ggsn
# Answer: The library(ggsn) is a package in R that is used to create North symbols and scale bars for maps. These symbols are created using "ggplot" and "ggmap" commands.
      # : The package adds north symbols (there are 18 North Symbol Options), and scale bars (units = Km) to maps, using metric or geographic coordinates.
      # : This is done using the commands "ggplot" and "ggmap" 

             