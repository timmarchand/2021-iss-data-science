## Processing random questions data

## OPTIONAL clear memory
 rm(list=ls(all=TRUE))

## load libraries and data ----
# library(tidyverse)
# library(lubridate) # for working with dates


## Efficient alternative - use pacman!
pacman::p_load(tidyverse, lubridate, janitor)

## load data from google form


dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQul_g9GtsSj-30sQ2hOE_rixxy8gH6AdhawcT5AoRJodJdGZnBgqtwgDAmfab0CJ1AjmFCrKmVw5MA/pub?gid=1799592268&single=true&output=csv")

glimpse(dat)

## These names are not ideal!
## One option is to use the janitor::clean_names() function

dat %>% 
  clean_names()

## How did the names change?

## Alternatively, let's rename these columns into something usable

cols <- c("timestamp", # time of submission
             "height", # height in cm
             "month", # birth month
             "date", # birth date
             "visit1", # first Q about visiting Africa
             ">14", # More than 14 countries Q
             "guess1", # number of countries in Africa guess
             "visit2", # second Q about visiting Africa
              "<96",  # Fewer than 96 countries Q
             "guess2", # number of countries in Africa guess 2
             "data_sci") # data science course memeber

## Use set_names()

dat <- dat %>%
  set_names(cols)
dat



## What kind of data variables do we have?
glimpse(dat)

## Tidy up the data ----

## OBJECTIVES

#' Change data_sci into binary

#' Combine guess1 and guess2
#' Change guess into numeric
#' Combine visit1 and visit2
#' Change visit into binary

#' Convert timestamp into date_time format
#' Combine month and date for birthday variable
#' Find out the "odd" birthdays, new binary variable
#' Create new variable for this year's birthday

## Lets start by mutating data_sci column to a binary
## TRUE / FALSE

dat <- dat %>%
  mutate(data_sci = str_detect(data_sci,"Yes"))
dat

#' What does this code do?


## Unite the two visit and guess questions into one
## mutate visit into logical TRUE / FALSE (hint, use str_detect)
## mutate guess into numeric  (hint, use parse_number)

dat <- dat %>% 
## Unite the two visit and guess questions into one
  unite("visit", c(visit1,visit2))
## mutate visit into logical TRUE / FALSE (hint, use str_detect)
## mutate guess into numeric  (hint, use parse_number)

dat

## Tidy up the dataframe 2 ----
## convert timestamp to date_time format with mdy_hms()
dat <- dat %>%
  mutate(timestamp = lubridate::mdy_hms(timestamp))
## create "odd", TRUE if date is odd number (hint use parse_number and %%)
## unite month and date to create birthday
## create birthday_24 (hint use str_c() and mdy())

dat
## Tidy up the dataframe and save ----
## Select only relevant columns for analysis
dat <- dat %>%
  select(-c(">14", "<96")) 

## save data into data folder

write_csv(dat, file = "data/random_qs.csv")

## Quick exploratory analysis  ----

### create 2 data frames
## add case numbers

rq_all<- dat %>%
            mutate(case = row_number(), .before = timestamp)

rq_class <- dat %>%
            filter(data_sci) %>% 
            mutate(case = row_number(), .before = timestamp)

## Get mean across everything
rq_class %>%
  summarise(across(everything(), mean))

## How does mean work with all variable types?


## summary function also useful
rq_class %>%
  summary()

## Now compare with rq_all

## Use dplyr code to find out

## Which year generated the most responses?
## How many respondents shared a birthday?
