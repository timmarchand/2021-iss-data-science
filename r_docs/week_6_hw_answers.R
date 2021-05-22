### Set up ----
### Clear memory
rm(list=ls(all=TRUE))


# load libraries ----------------------------------------------------------
library(tidyverse)
# install.packages("ggpubr") # once for your machine
library(ggpubr) # for statistics
library(gridExtra) # for arranging plots






# Read in and select data ------------------------------------------------------------
dat <- read_csv("data/africa_guess.csv")

# Check and Wrangle data --------------------------------------------------------------
df_all <- dat %>% filter(height >7)

df_class <- df_all %>% filter(df_all$datasci)

# Visualise data ----------------------------------------------------------

## Let's compare the plots from last week with this week's HW side-by-side
## Starting with the density plots
## height vs birthday
dp1 <- ggplot(df_all) +
  aes(x = height, fill = birthday) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=birthday)) +
  labs(x = "Height in cm", y = "Probability Density", title = "Are people's heights related to birthday dates?", fill = "birthday date") +
  theme_minimal()
## guess vs birthday
dp2 <- ggplot(df_all) +
  aes(x = guess, fill = birthday) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=birthday)) +
  labs(x = "Guess for how many countries in Africa", y = "Probability Density", title = "Are guesses related to birthday dates?", fill = "birthday date") +
  theme_minimal()
gridExtra::grid.arrange(dp1,dp2)

## Try the boxplot again
## height vs birthday
bp1 <- ggplot(df_all) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  theme_minimal()
## guess vs birthday
bp2 <- ggplot(df_all) +
  aes(x = "", y = guess, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  # ggpubr::stat_compare_means(method = "t.test") +
  theme_minimal()
gridExtra::grid.arrange(bp1,bp2, nrow =1)
## Do these plots suggest a difference in the groups?
## Are the differences significant?
## Later we will look at statistical testing in detail, but for now
## Use the ggpubr package which prints statistical results onto the plot!
## For comparing numerical results of categorical data
## use compare_means
bp1 <- ggplot(df_all) +
  aes(x = "", y = height, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Height in cm", title = "Boxplot of height by odd/even birthday") +
  ggpubr::stat_compare_means(method = "t.test") +
  theme_minimal()
bp2 <- ggplot(df_all) +
  aes(x = "", y = guess, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  ggpubr::stat_compare_means(method = "t.test") +
  theme_minimal()
gridExtra::grid.arrange(bp1,bp2, nrow =1)
## We have a non-significant difference for height (as expected)
## But a significant difference for guesses! How can that make sense?


## scatter plots with correlations ----
## simple scatter plots
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  theme_minimal()
ggplot(df_all) +
  aes(x = height, y = guess) +
  geom_point() +
  theme_minimal()
## use geom_smooth to add a line of a model fitting the data
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth()  +
  theme_minimal()
ggplot(df_all) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth()  +
  theme_minimal()
## these lines are not straight! the model is probably overfitting the data
## use geom_smooth and the linear method to get a straight regression line
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm")  +
  theme_minimal()
ggplot(df_all) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method = "lm")  +
  theme_minimal()
## the grey parts indicate 95% confidence level bands
## you can remove them by setting the standard error to false
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm" , se = FALSE)  +
  theme_minimal()
ggplot(df_all) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  +
  theme_minimal()
## finally, use ggpubr correlation stat to get R and p values
ggplot(df_class) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method ="lm")  +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()
ggplot(df_all) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method = "lm")  +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()
## As expected, no significant correlations!

