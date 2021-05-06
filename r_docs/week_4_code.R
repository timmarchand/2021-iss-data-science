
# load libraries ----------------------------------------------------------
library(tidyverse)

# Read in and select data ------------------------------------------------------------
dat <- read_csv("https://tinyurl.com/iss-random")

# Check and Wrangle data --------------------------------------------------------------
glimpse(dat)
View(dat)

# datasci = member of this CLIL seminar or not
# height  = height in cm
# month = month of birthday
# date = date of birthday
# visit = visited Africa or not
# guess = guess of number of countries in Africa


# convert dates to numerical column
dat <- dat %>%
  mutate(date = parse_number(date))

# convert months to numerical column
dat <- dat %>%
  mutate(month = match(month, month.name))

## add column for odd vs even birthday dates
dat$birthday <- ifelse(dat$date %% 2,"odd","even")

## tidyverse method
# dat <- dat %>%
#   mutate(birthday = ifelse(date %% 2,"odd","even"))


## count certain categories
dat %>% count(datasci)
dat %>% count(birthday)


## subset CLIL members data
africa_guess_class <- dat[dat$datasci,]

## Tidyverse method
# africa_guess_class <- dat %>% filter(datasci)


glimpse(africa_guess_class)
glimpse(africa_guess_all)




# Visualise data ----------------------------------------------------------

ggplot(africa_guess_all, aes(height,guess)) +
  geom_point()



