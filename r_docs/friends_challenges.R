## Optional clearing of Environment
# rm(list = ls())

# load libraries ----
pacman::p_load(tidyverse,glue)


# load data
friends <- read_csv("https://tinyurl.com/iss-friends-web")
## clean up with str_squish
friends$plot <- str_squish(friends$plot)


# REGEX list ----
## You will need to input some of these REGEX in the code below
## Try to work out what each one matches
# "^\\d\\.0" >> string starting with any number followed by a period and zero (e.g "3.0")
# "^The\\sOne"
# "^@@@"
# "@@@.+$"
# "stars$" 
# "\\S+"
# "([^\\d]+)(\\d.+)"
# "([^ ]+\\s[^ ]+\\s[^ ]+).*$"
# "([0-9]{4})-([0-9]{2})-([0-9]{2})"
# "\\2"
# "^(\\w+\\s\\w+\\s\\w+)"
# "^([A-z]+\\s[A-z]+\\s[A-z]+).*$"
# "\\2/\\3/\\1"
# "\\3/\\2/\\1"
# "\\b[a-z]{2,}ly\\b"

## Using the friends dataframe, can you work out the plots featuring which of the main characters? ----
## ("Monica","Rachel","Ross","Chandler","Joey","Phoebe") had the highest average rating?


## Try 1: Use filter / str_detect / summarise to get means for each character

friends %>% filter(str_detect(plot,"Monica")) %>% summarise(mean =mean(rating))
friends %>% filter(str_detect(plot,"Rachel")) %>% summarise(mean =mean(rating))

## Programming rule - if you use copy and paste more than twice, there is probably a simpler method

## Try 2: Use "[" to subset instead, put all the names in one summary table]

friends %>% summarise(mean_Monica = mean(rating[str_detect(plot,"Monica")]),
                      mean_Rachel = mean(rating[str_detect(plot,"Rachel")]),
                      mean_Ross = mean(rating[str_detect(plot,"Ross")]),
                      mean_Chandler = mean(rating[str_detect(plot,"Chandler")]),
                      mean_Phoebe = mean(rating[str_detect(plot,"Phoebe")]),
                      mean_Joey = mean(rating[str_detect(plot,"Joey")]))

## Advantage, now you can copy and paste and try the titles instead:


## ADVANCED: Functional approach - create a function 
chr_means <- function(name){
  friends %>% filter(str_detect(plot,glue("{name}"))) %>%  # use {} from glue package 
  summarise("{name}_av" := mean(rating)) # use {} and := from glue package
}

## check that it works
chr_means("Monica")

## create a vector of character names
names <- c("Monica","Rachel","Ross","Chandler","Phoebe","Joey")

## Use purr::map family to iterate on the new function on the character list
map(names,chr_means)

## map returns a list, use map_dfc to return a dataframe with columns
map_dfc(names,chr_means)


## Have a look at the titles. Can you find any common patterns? How would you group them by their titles? ----
## Can you summarise any information after grouping the titles by pattern?

## Pull out a random sample of titles
qwe <- friends %>%  sample_n(50) %>% pull(title)
qwe
## Do all titles start with "The One"?

## use see() to check your regex pattern
see("_____", string = qwe)

## Use !str_detect to find all non-matches
friends %>% filter(!str_detect(string = title, pattern = "_____")) %>% pull(title)

# common pattern seems to be "The One with..." and "The One Where..."
# How many of each?
friends %>% filter(str_detect(string = title, pattern = "^The\\s\\One\\swith")) %>% count()
friends %>% filter(str_detect(string = title, pattern = "^The\\s\\One\\sWhere")) %>% count()

## Count to see how many different title types there are 
# Create a new variable called title_type, replace the title with just the first 3 words,

## use see() to check your regex pattern
see("_____", string = qwe)


friends %>% 
  mutate(title_type = str_extract(string = title, pattern = "_____")) %>% 
  count(title_type, sort = TRUE) 

## If you find \\w+ hard to read, you can also use [A-z]+ or [^ ]+ to represent words
## Note that [A-z]+ will not match any punctuation marks
friends %>% 
  mutate(title_type = 
           str_extract(string = title, pattern = "^([A-z]+\\s[A-z]+\\s[A-z]+)")) %>% 
  count(title_type, sort = TRUE) 

## Summarise to see which title types have the highest average rating 
friends %>% 
  mutate(title_type = 
           str_extract(string = title, pattern = "^([A-z]+\\s[A-z]+\\s[A-z]+)")) %>% 
  group_by(title_type) %>% 
  summarise(av_rating = mean(rating), av_vote = mean(votes), n = n())


## Finding special themes ----
## You can select text around certain themes or words



travel <- "airplane|train|taxi|bus|car|road"
cities <- "New York|London|Paris|Los Angeles"

adverbs <- "\\b[a-z]{2,}ly\\b"

events <- c("wedding", "funeral", "party", "birthday") 
work <- c("job", "work", "office", "employer")

## Find plots where certain cities are mentioned:
## Create a vector of plots where cities are mentioned:
asd <- friends %>% 
  filter(str_detect(plot,cities)) %>% # cities is a length 1 object with OR pipes
  pull(plot)

## check with see()
see(cities, string = asd)

## Now do the same for travel and adverbs

## In the case of a vector of strings, collapse to single length

events <- str_c(events, collapse = "|")

zxc <- friends %>% 
filter(str_detect(plot, event))) %>% 
  pull(plot)

## check with see()
see(events, string = zxc)

## Now do the same for cities and check with see()



