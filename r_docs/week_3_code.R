## Clear the workspace and load the packages ----
## It is a good idea to start a new R session with a blank slate

# this code removes everything from your workspace
rm(list=ls())
## Load the packages
## you may need to use install.packages() first
library(nycflights13)
library(ggplot2)
library(dplyr)

# Scatterplots allow you to visualise the relationship between two numerical variables
## Using the flights dataframe included in the  nycflights13 package, 
# let's visualise the relationship between departure delay on the x axis and arrival delay on the y axis 
# for all Alaska Airlines flights.

## Filter only the Alaskan airlines flights using dplyr functions, create a new df from the result
alaska_flights <- flights %>% 
  filter(carrier == "AS")


## Question: Compare the new df (alaska_flights) with the original (flights) using View() or glimpse(). ----
# How are they different? 
glimpse(flights)
glimpse(alaska_flights)

View(flights)

## Scatterplots using geom_point
## Most ggplots are built up with lines of code, each responding to a layer of the graph. 
# The first line includes the functiom itself, and you specify the data you want to use, 
# and what kind of aesthetic mapping you want - 
# which means what should be on the x axis and what on the y axis.

## First line of code - but no layers!
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay))


## We add a layer to the ggplot() call using the + sign at the end of a line
## For scatter plots, the geometric object you want to use is geom_point()
x11()
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point()

## Questions: What kind of relationship exists between dep_delay and arr_delay? Are you surprised? Why? Why not? ----
## Why do you believe there is a cluster of points near (0, 0)? What does (0, 0) correspond to in terms of the Alaska Air flights?

## Overplotting ----
## Because there are a lot of points near 0,0 on the graph, it is hard to tell the true number of points - this is called overplotting.

## Two ways to deal with it: adjust the transparency of the points by setting the alpha argument in geom_point
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.2) # 0.2 represents 20% transparency

## The darker points now show where there is overplotting



## second option, use jitter to nudge the points slightly apart
## note the code is very similar to geom_point above!
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_jitter(width = 30, height = 30) # use width and height to control how much to jitter


## Question: After using the alpha argument, guess the approximate range of arrival delays and departure delays that occur most frequently. Could you make the same guess without the transparent points? ----

## Extra pracice on scatterplots and the nettle data ----

## Nettle (1999) discusses the interesting idea that linguistic diversity is correlated with climate factors:
## Countries with more fertile land have more languages than those with more ecological risk, because local
## people have less need to travel and so local languages develop over time. Nettle measured ecological risk
## by mean growing season (MGS column in the data), which is how many months per year one can grow crops.


## Don't forget to read in the data from somewhere (read_csv()).
nettle <- readr::read_csv("data/nettle.csv")
## Add your own comments as you like!
ggplot(nettle, aes(x = MGS, y = Langs)) +
  geom_point()

# Swapping geom_point() with geom_text() returns an error:

ggplot(nettle, aes(x = MGS, y = Langs)) +
  geom_text()

# Because this geom needs an additional mapping, it ...
# ...needs to know which column contains the text to plot!

ggplot(nettle, aes(x = MGS, y = Langs, label = Country)) +
  geom_text()

# To save an open ggplot, just type:

ggsave('figures/nettle.png', width = 8, height = 6)

# To create a double-plot, first save each plot:

plot1 <- ggplot(nettle) +
  geom_point(mapping = aes(x = MGS, y = Langs))

plot2 <- ggplot(nettle,
                aes(x = MGS, y = Langs, label = Country)) +
  geom_text()

# Then use 'gridExtra' to create double-plot:

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)



## Linegraphs ----
# Linegraphs show the relationship between two numerical variables when the variable on the x-axis 
# is sequential - this usually means you often see time on the x-axis.
# Let’s illustrate linegraphs using another dataset in the nycflights13 package: the weather data frame.

## Explore the weather data frame by running View(weather) and glimpse(weather).
## Checkout the associated help file by running ?weather to bring up the help file.

# There is a variable called temp of hourly temperature recordings in Fahrenheit at weather stations 
# near airports in NYC: Newark (origin code EWR), John F. Kennedy International (JFK), and LaGuardia (LGA).

# Let's focus on early January temperatures in Newark, by filtering and creating a new df 
early_january_weather <- weather %>% 
  filter(origin == "EWR" & month == 1 & day <= 15)

## Question: Compare the new df with the original using View() or glimpse(). ----
# How are they different? 

glimpse(early_january_weather)





## Linegraphs via gemom_line----
ggplot(data = early_january_weather, # using our new dataframe as the data
       mapping = aes(x = time_hour, y = temp)) + # set x-axis as time_hour variable, and y to temperature
  geom_line() # added a line as the geometric layer of the plot 




## Histograms ----
## Histograms are a great way to see how a variable is distributed:
# 1. What are the smallest and largest values?
# 2. What is the “center” or “most typical” value?
# 3. How do the values spread out?
# 4. What are frequent and infrequent values?

# A histogram is a plot that visualizes the distribution of a numerical value as follows:

# 1. We first cut up the x-axis into a series of bins, where each bin represents a range of values.
# 2. For each bin, we count the number of observations that fall in the range corresponding to that bin.
# 3. Then for each bin, we draw a bar whose height marks the corresponding count.

# Unlike with scatterplots and linegraphs, there is now only one variable being mapped in aes(): 
# a single numerical variable. The y-aesthetic of a histogram, the count of the observations in each bin, 
# gets computed for you automatically.


## Let's look again at how the temp variable is distributed in the weather df
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram()

# Warnings: 
# R chose 30 bins by default
# One NA value was removed

## Try again, but add white borders to the bins to make it easier to see
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(color = "white")

## Change the color of the bars using the fill argument
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(color = "white", fill = "steelblue")


## Adjusting the bins of a histogram ----
# Two ways to adjust the bins in a histogram:

# 1. By adjusting the number of bins via the bins argument to geom_histogram().
# 2. By adjusting the width of the bins via the binwidth argument to geom_histogram().

# 1. Tell R how many bins to use, override the default 30 bins
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(bins = 40, color = "white")


#2. Tell R the width of the bin (i.e. how many degrees temperature in the range of the bin)
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "white")

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_density()

## Density plots as an alternative ----
# A density plot is a representation of the distribution of a numeric variable. 
# It shows the probability density function of the variable, and can be seen like to be 
# a smoothed version of the histogram and is used in the same concept. 

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of temperatures around New York airports") +
  theme_minimal()


## Facets ----
# Faceting is used when we’d like to split a particular visualization by the values of another variable. 
# This will create multiple copies of the same type of plot with matching x and y axes, 
# but whose content will differ.

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month) # note the tilde symbol - found next to the = sign on many Japanese keyboards

# Tell R the number of rows or columns you want using the nrow or ncol arguments
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month, nrow = 4)

# Question: What do all the numbers represent on the plot? ----

## Boxplots ----

# Faceted histograms are one type of visualization used to compare the distribution of a numerical variable split by the values of another variable
# Another type of visualization that achieves this same goal is a side-by-side boxplot

# Let's try looking at temperatures by month in the weather data:
ggplot(data = weather, mapping = aes(x = month, y = temp)) +
  geom_boxplot()

# Warning messages:
#   1: Continuous x aesthetic -- did you forget aes(group=...)? 
#   2: Removed 1 rows containing non-finite values (stat_boxplot). 

# This means we have a continuous, numerical variable on the x axis - but it needs to be categorical.
# In R, categorical variables are called factors, and you can simply convert the months using factor().

ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()

# The “box” parts represent the 1st quartile, the median (the 2nd quartile), and the 3rd quartile.

# The height of each box (the value of the 3rd quartile minus the value of the 1st quartile) is the interquartile range (IQR). 
# It is a measure of the spread of the middle 50% of values, with longer boxes indicating more variability.

# The “whisker” portions of these plots extend out from the bottoms and tops of the boxes and 
# represent points less than the 25th percentile and greater than the 75th percentiles, respectively. 
# The length of these whiskers show how the data outside the middle 50% of values vary, with longer whiskers indicating more variability.

# The dots representing values falling outside the whiskers are called outliers. 



