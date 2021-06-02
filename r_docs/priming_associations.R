## OPTIONAL clear memory
# rm(list=ls(all=TRUE))


# load libraries ----------------------------------------------------------
pacman::p_load(tidyverse, ggpubr, vcd, broom, moderndive, sjPlot, gapminder)

# Read in and select data ------------------------------------------------------------
df <- priming <- read_csv("https://tinyurl.com/iss-priming")

# for later reference
write_csv(priming,"data/priming.csv")

# Check and Wrangle data --------------------------------------------------------------
## continuous vs categorical:  priming data----

## count_task vs candy

## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)

# check the data
glimpse(df)

## Copy and paste the code from the variable_associations file
## Use find and replace to adjust the code accordingly

## continuous vs categorical:----
## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)


## categorical vs categorical ----
## compare joint frequencies in contingencies tables
## statistical test from the Chi-squared family
## plot joint frequencies with bar plots or mosaic plots


## continuous vs continuous ----
## compare values with scatter plots
## assess association with regression lines
## statistical tests from the R-squared family (or the linear model of regression)
