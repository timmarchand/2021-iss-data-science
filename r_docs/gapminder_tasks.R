### These sets of tasks can be completed at any time, whenever you feel you can answer them
## Add your solutions to the last Exercise in any Problem Set

# load libraries
library(tidyverse)

# load the data
# with a relative path
gapminder <- read_csv("data/gapminder.csv")
# from an online google sheet
gapminder <- read_csv("https://tinyurl.com/iss-gapminder-web")


#   Life expectancy set ####

# find the mean life expectancy of all countries in the dataset in 1952 and 2007
# find the minimum, maximum, median life expectancy of countries in Africa in 2007
# make a graph showing the changes of mean life expectancy for the countries grouped by continent (x-axis) and by year (y-axis)
# determine whether there is a statistically significant difference between the mean life expectancy values in Japan and Brazil in the dataset 


# Continent set ####

# find out how many countries are in each continent
# find out the percentage of people who lived in Europe in 1952
# find out the population percentages for all countries in 2007
# make a graph comparing the gdp per capita for all the countries in each continent


# GDP per capita set ####

# find out which countries had the minimum and maximum gdp in 1952 and 2007
# find out whether gdp values are normally distributed
# convert the gdp data into categorical data type: high income - medium income - low income
# do a transformation of the gdp values to make them normally distributed

# Population set ####

# find out how many countries had a larger population than Japan in 1952 and 2007
# find the mean and standard deviation of population for each continent in 2007
# find out which country had the biggest change in population between 1952 and 2007 - in absolute terms and relative terms
# make a graph plotting population (x axis) against life expectancy (y axis) - is there any correlation?