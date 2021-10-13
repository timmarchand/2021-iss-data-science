### Set up ----
### Intsructions
## Complete this code file using the copy-paste-tweak method for the following:

## 1 Reading in and selecting the data - remember you need  two sets for df_all and df_class

## 2 Check and wrangle the data -> - don't forget the outlier!

## 3 Complete the visualisation process at all the DIY points



### Clear memory
rm(list=ls(all=TRUE))


# load libraries ----------------------------------------------------------
library(tidyverse)
# install.packages("ggpubr") # once for your machine
library(ggpubr) # for statistics
library(gridExtra) # for arranging plots



# Read in and select data ------------------------------------------------------------


# Check and Wrangle data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------

## Let's compare the plots from last week with this week's HW side-by-side
## Starting with the density plots
## height vs birthday 
dp1 <- ggplot(df_class) +
  aes(x = height, fill = birthday) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=birthday)) +
  labs(x = "Height in cm", y = "Probability Density", title = "Are people's heights related to birthday dates?", fill = "birthday date") +
  theme_minimal()
## DIY n_africa vs birthday df_all ####

dp2

gridExtra::grid.arrange(dp1,dp2)

## Try the boxplot again
## height vs birthday
bp1 <- ggplot(df_class) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  theme_minimal()
## DIY n_africa vs birthday df_all ####

bp2 <- ggplot(df_all) +


gridExtra::grid.arrange(bp1,bp2, nrow =1)
## Do these plots suggest a difference in the groups?
## Are the differences significant?
## Later we will look at statistical testing in detail, but for now
## Use the ggpubr package which prints statistical results onto the plot!
## For comparing numerical results of categorical data
## use compare_means

bp1 <- ggplot(df_class) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  ggpubr::stat_compare_means(method = "t.test") +
  theme_minimal()

## DIY plots with stats -  df_all ####
bp2 <- ggplot(df_all) +

gridExtra::grid.arrange(bp1,bp2, nrow =1)
## We have a non-significant difference for height (as expected)
## But a significant difference for guesses! How can that make sense?


## scatter plots with correlations ----
## simple scatter plots - class
ggplot(df_class) +
  aes(x = height, y = n_africa) +
  geom_point() +
  theme_minimal()

## DIY simple scatter plots - df_all
ggplot(df_all) +


## use geom_smooth to add a line of a model fitting the data
## model line - class
ggplot(df_class) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth()  +
  theme_minimal()

## DIY model line -  df_all ####
ggplot(df_all) +

## these lines are not straight! the model is probably overfitting the data
## use geom_smooth and the linear method to get a straight regression line
## linear fit - class
ggplot(df_class) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method ="lm")  +
  theme_minimal()

## DIY linear fit -  df_all ####
ggplot(df_all) +

## the grey parts indicate 95% confidence level bands
## you can remove them by setting the standard error to false
## linear fit - no standard error - class
ggplot(df_class) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method ="lm" , se = FALSE)  +
  theme_minimal()

## DIY linear fit - no standard error -  df_all ####

## finally, use ggpubr correlation stat to get R and p values
## print statistics - class
ggplot(df_class) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method ="lm")  +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## DIY print statistics -  df_all ####



## Are there any significant differences?
