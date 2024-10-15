
### Set up ----

### Clear memory (optional)
# # rm(list=ls(all=TRUE))

### load packages

# library(tidyverse) # for readr, ggplot2 and dplyr
# library(gridExtra) # for arranging plots

pacman::p_load(tidyverse, gridExtra)


# MONTE CARLO SIMULATIONS IN R  --------------------------------------------

## Also known as bootstrapping
## Very first baby steps in Machine Learning!

## 1 SAMPLING AND REPLICATING ----
# make a vector of names
# Change these names to match this class members names!
names <- c("member_1","member_2","member_3","member_4")
names
# use sample to pick two names at random
sample(names, size = 2, replace = FALSE)

# set replace = TRUE to put names back in the hat
sample(names, size = 5, replace = TRUE)

## You may have different results because it is random!
## We can control that by setting the seed - this regularises random number generation
## Set seed
set.seed(111)
sample(names, size = 2, replace = FALSE)
sample(names, size = 5, replace = TRUE)

## What could this represent?
sample(1:6,1) # no need for replace argument as only taking one sample

## to represent rolling 2 dice
sample(1:6, size = 2, replace = TRUE)

## save as a variable score
roll <- sample(1:6, size = 2, replace = TRUE)
roll
## to get the total, use sum()
sum(roll)

## same as above, but in one line
sum(sample(1:6, size = 2, replace = TRUE))


## create a simple function to represent rolling 2 dice and adding them
two_dice <- function(){ # use curly brackets open on the same line as function()
  roll <- sample(1:6, size = 2, replace = TRUE) # create a variable within the function
  return(sum(roll)) # this is the value returned
} # close on the last line by itself
two_dice()
### Use replicate() to repeat a function
replicate(n = 3, getwd()) ## replicating the getwd() function 3 times

## repeat our two_dice function nine times
replicate(n = 4, two_dice())

# save as a vector
scores <- replicate(n = 4, two_dice())
scores
# average of scores?
mean(scores)
median(scores)

# how diverse are the scores?
range(scores) # returns the lowest and highest values
table(scores) # get a frequency table of scores
sd(scores) # standard deviation - useful for statistics
hist(scores) # simple histogram

## DIY 1
##Experiment with changing the n value and plotting  simple histograms
scores <- replicate(n = , two_dice())
hist(scores)


# 2 More plotting simulations ----

# simple histogram of scores
hist(scores) # same as our frequency table

# why not use ggplot?
ggplot(data = scores, aes(x=scores)) +
  geom_histogram()  # Error: `data` must be a data frame!

# lets make a df combining names and scores to use with ggplot
scores <- replicate(n = 4, two_dice())
df <- tibble(name = names,
             score = scores)

# now we can use ggplot to make the histogram
ggplot(data = df, aes(x=score)) +
  geom_histogram(binwidth = 1, colour = "white") +
  scale_x_continuous(breaks = 0:12, limits = c(1,12)) # sets the x axis

## ...in case you are curious about your own score!
ggplot(data = df, aes(x=name, y=score)) +
  geom_col() + # geom_col creates a simple bar chart
  scale_y_continuous(breaks = 0:12, limits = c(0,12)) # sets the y axis

## Simulating several rolls of the dice

## another function, this time combining replicate() and two_dice()
test_2_dice <- function(n){ # with an input variable n + number of replications
  test <- tibble(roll = 1:n,
                 score =replicate(n,two_dice()))
  return(test)
}

t_5<- test_2_dice(5)
t_50<- test_2_dice(50)
t_5000 <- test_2_dice(5000)

mean(t_5$score)
mean(t_50$score)
mean(t_5000$score)

median(t_5$score)
median(t_50$score)
median(t_5000$score)


## because test_2_dice creates a df, you can put the result straight into ggplot
test_2_dice(20) %>% # this is the piping operator - easily confused with +!
  ggplot() +
  aes(x = score) +
  geom_histogram(binwidth = 1, colour = "white")


## in fact, you can even add plotting when making a function
plot_2_dice <- function(n){
  test <- tibble(throw = 1:n,
                 score = replicate(n,two_dice()))
  ggplot(test) +
    aes(x = score) +
    geom_histogram(binwidth = 1L, fill = "#0c4c8a", colour = "white") +
    scale_x_continuous(breaks = 0:12, limits = c(0,14)) +
    ggtitle(paste("Histogram after",as.character(n),"rolls", sep = " ")) +
    theme_minimal()
}

## play around with the plot_2_dice() function, changing the values of n
## what is the trend for the higher the value of n?
plot_2_dice()


## Let's check our intuition by comparing the histograms with the normal distribution
## full plotting with density plot and normal distribution comparison

norm_2_dice <- function(n){
  # same code to run the test
  test <- tibble(throw = 1:n,
                 score = replicate(n,two_dice()))
  # plot the histogram for first plot, p1
  p1 <- ggplot(test) +
    aes(x = score) +
    geom_histogram(binwidth = 1L, fill = "#0c4c8a", colour = "white") +
    scale_x_continuous(breaks = 0:12, limits = c(0,14)) +
    ggtitle(paste("Histogram after",as.character(n),"rolls", sep = " ")) +
    theme_minimal()

  # plot a density plot for second plot, p2
  p2 <-ggplot(test) +
    aes(x = score) +
    geom_density( fill = "#0c4c8a", alpha = 0.5, adjust = logb(n,50)) + # adjust the density bandwidth according to sample size
    scale_x_continuous(breaks = 0:12, limits = c(0,14)) +
    ggtitle("Density plot vs Normal distribution") +
    theme_minimal() +
    # add normal distribution based on mean and sd
    stat_function(colour = "red",
                  fun = dnorm,
                  args = list(mean = mean(test$score), sd = sd(test$score)))
  # put the plots together in one row
  grid.arrange(p1,p2,nrow=1)
}

## check it out to see how it works
norm_2_dice(10)

### Creating a number of plots with the number of rolls
### Set to 5, 50, 500, 5000, 50000

# define a number of rolls for input into map()
# note that map() requires a list
rolls <- as.list(c(5,50,500,5000,50000))
# Create a list of plots based on the rolls
plots <- map(rolls, ~norm_2_dice(.x) )
# arrange plots with grid.arrange
do.call(grid.arrange, c(plots, list(ncol=1)))





# FURTHER MONTE CARLO EXAMPLES -----------------------------------------------------------


## 1 GUESSING PI ----
# set the seed for reproducibility
set.seed(314)
# number of simulations
n=1000000
# generate the x and y co-ordinates from uniform
# distribution taking values from -1 to 1
x<-runif(n, min=-1, max=1)
y<-runif(n, min=-1, max=1)
# Distrance of the points from the center (0,0)
z<-sqrt(x^2+y^2)

# the area within the circle is all the z
# which are smaller than the radius^2
# in our case radius=1
4*sum((z<=1))/length(z)

InOut<-as.factor(ifelse(z<=1, "In", "Out"))
plot(x,y, col=InOut, main="Estimate PI with Monte Carlo")

## 2 BIRTHDAY PARADOX ----

## R actually has a function which calculates the probability of people in a group sharing a birthday
?pbirthday

## probability of > 2 people with the same birthday in a group of 23
pbirthday(n = 23, coincident = 2)
pbirthday(n = 100, coincident = 2)

## Without knowing the mathematics behind this, we can use monte carlo simulation

## create group size from 2 to 100, even numbers only
people = seq(2, 100, 2)
## create 10000 random trials
trial = 1:10000
## cross each over into new df
trials <- crossing(trial,people)
trials
## calculate chance

## take the large df
chance_summary <- trials %>%
  ## add birthday list column with a random sample of days
  mutate(birthday = map(people, ~ sample(365, ., replace = TRUE)),
         ## find out if any are duplicated
         multiple = map_lgl(birthday, ~ any(duplicated(.)))) %>%
  ## group by size of group
  group_by(people) %>%  ## use mean of logic vector to calculate probablilty
  summarize(chance = mean(multiple))


## Visualizing the probability -------------------------------------------
ggplot(chance_summary, aes(people, chance)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Probability two have the same birthday")

## Compare simulated with mathematical probabilities
probs <- tibble(people = people,
                        math = map_dbl(people,stats::pbirthday),
                        sim= chance_summary$chance,
                        difference = math - sim)


p <- probs%>%
  ggplot(aes(people, sim)) +
  geom_line() +
  theme_minimal()

p
p + geom_line(aes(y = math, color = "red"))

probs %>% ggplot(aes(people,difference)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()




# Further Reading ---------------------------------------------------------

## About summary statistics in R

## Find out about some other summary functions
# from this page: https://www.statsandr.com/blog/descriptive-statistics-in-r/#advanced-descriptive-statistics
