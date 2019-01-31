# Day 4
# Exercise 1 : Convert a CSV File to RData file Type
# Nicole Okkers
# 31 January 2019

# Conversion from CSV to RData file type
Lam <- read_csv("data/laminaria.csv") # Read the CSV file for laminaria
save(Lam,file = "data/laminaria.RData") # Converting from CSV to RData


