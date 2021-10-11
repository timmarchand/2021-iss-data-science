### Set up ----
### Intsructions
## Complete this code file using the copy-paste-tweak method for the following:

## 1 Reading in  the data

## 2 Check and wrangle the data  - remember you need  two sets for df_class and df_all - and don't forget the outlier!

## 3 Complete the visualisation process at all the DIY points



### Clear memory
rm(list=ls(all=TRUE))


# load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr) # for statistics
library(gridExtra) # for arranging plots



# Read in and select data ------------------------------------------------------------


# Check and Wrangle data --------------------------------------------------------------

# Create two dataframes: 
## df_class for iss data science class members
## df_all for the whole data set BUT removing the outlier

# Visualise data ----------------------------------------------------------

## Let's compare the plots from the Rmd file for df_class with df_all side-by-side
## Starting with the density plots

## height vs birthday df_class
dp1 <- ggplot(df_class) +
  aes(x = height, fill = birthday) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=birthday)) +
  labs(x = "Height in cm", y = "Probability Density", title = "Are people's heights related to birthday dates?", fill = "birthday date") +
  theme_minimal()

## DIY height vs birthday df_all (copy, paste and tweak the code from above)----
dp2 <- 


gridExtra::grid.arrange(dp1,dp2)

## Try the boxplot again
## height vs birthday df_class
bp1 <- ggplot(df_class) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  theme_minimal()
## height vs birthday df_all ----
bp2 <- 

  
  
gridExtra::grid.arrange(bp1,bp2, nrow =1)

## Try the boxplot again with stats

##  plots with stats - df_class 
bp1 <- ggplot(df_class) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  ggpubr::stat_compare_means(method = "t.test") +
  theme_minimal()

## DIY plots with stats - df_all ---- 
bp2 <- ggplot(df_all) +
  
  
  

gridExtra::grid.arrange(bp1,bp2, nrow =1)

## scatter plots with correlations ----
## simple scatter plots - class
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  theme_minimal()

## DIY simple scatter plots - all ----
ggplot(df_all) +


## use geom_smooth to add a line of a model fitting the data

ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm")  +
  theme_minimal()

## DIY linear fit - all ----
ggplot(df_all) +


## linear fit - no standard error - df_class
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm" , se = FALSE)  +
  theme_minimal()

## DIY linear fit - no standard error - df_all ----

## finally, use ggpubr correlation stat to get R and p values
## print statistics - df_class
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm")  +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## DIY print statistics - df_all ####



## Final challenge which code from above should you copy, paste and tweak to see if someone's birthday as a predictor (odd or even) influneces the n_africa variable as an outcome?

## Create two graphs - one for df_class, one for df_all and put them side-by-side
