## Load libraries ----
## Add your own comments if you remember what the packages are for
pacman::p_load(tidyverse, #
               rvest, #
               franc, #
               broom, #
               hunspell, # 
               zoo, # for rolling averages
               moderndive, #
               ggpubr) #

## set theme for ggplot
theme_set(theme_minimal())

## get grab_amazon_full() function
source("r_docs/grab_amazon_full.R")

## get data
ps4 <- read_csv("data/ps4_reviews.csv")
roomba <- read_csv("data/ps4_reviews.csv")

## CHALENGES ----

## CHALENGE: How accurately did the franc::franc identify languages? Find out how many reviews in English were misidentifed

## Are there any non-English in the UK?
roomba %>% filter(language != "eng" & country == "the United Kingdom")

## All seem to be English! Therefore, detection not perfect...
## Use hunspell::hunspell to check English spelling

roomba %>% filter(language != "eng") %>% 
  # note that hunspell needs to work on a vector, so we use pull()
  pull(review) %>% hunspell::hunspell() 


## returns a list with all the misspelled words (aka typos)
## using lengths() we can count how many typos are in each review
## A review with many typos could be non-English!
roomba <- roomba %>% 
  mutate(typos = hunspell(review) %>% lengths, .after = review)

## obviously if typos = 0, then most likely to be English
## How about > 0?

roomba %>% 
  filter(language != "eng", typos > 0) %>% 
  arrange(desc(typos)) %>% 
  View()

## CHALLENGE: Is there a correlation between word count and whether the review is considered helpful (for either ps4 or roomba or another product)?


## visualise

roomba %>% 
  ggplot(aes(review_wc,helpful)) +
  geom_point(alpha = 0.4) + # why set alpha? 
  theme(legend.position = "none")

## stats
roomba %>% 
  rstatix::cor_test(review_wc, helpful)

## Use Find in Files (Shift+Ctrl+F) to check where we used rstatix::cor_test before


## CHALLENGE: Does language of review have an effect on how helpful the review is considered (for either ps4 or roomba or another product)?

roomba %>%
  filter(language %in% c("eng", "sco", "fra")) %>%
  ggplot() +
  aes(x = helpful) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  facet_wrap(vars(language))


## problem of sparsity of data
## probably no useful model can be made
## let's try anyway, first by lumping some of the less common languages to "Other"

## Use forcats::fct_lump() to do this, experimenting with the n value

roomba %>% 
  mutate(language = fct_lump(language, n = 3)) %>% 
  count(language)


## Let's go with top 5
roomba_5_langs <- 
  roomba %>% 
  mutate(language = fct_lump(language, n = 5))

(mdl1 <- lm(helpful ~ language, data = roomba_5_langs))

get_regression_summaries(mdl1)

##  high p_value, but worse than that very low r_squared!


## CHALLENGE: Is there an effect of the rating score on how helpful the review is considered (for either ps4 or roomba or another product)?

roomba %>% 
  ggplot(aes(stars,helpful, color = as.factor(stars))) +
  geom_jitter() + # why jitter? alternatives?
  coord_flip()+
  theme(legend.position = "none")

(mdl2 <- lm(helpful ~ stars,roomba))

get_regression_summaries(mdl2)

##  high p_value, but worse than that very low r_squared!

## CHALLENGE: Can you visualise a trend over time for the ratings (for either ps4 or roomba or another product)?

ggplot(roomba) +
  aes(x = date, y = stars) +
  geom_line() 

## Very difficult to read the trend!

## Use a rolling average instead by getting a mean of a group of sequential values

roomba %>% 
  # organise by date
  arrange(date) %>%
  # create rolling mean variable for rating
  mutate(rolling_mean = zoo::rollmean(stars, k = 21, fill = NA)) %>%  # k is the number of successive values to group
  ggplot(aes(x=date, y=rolling_mean)) +
  geom_line() +
  
  labs(y = "Three-week rolling average rating")

# Note the warning message. Why were 20 rows removed?

## CHALLENGE: What is the relationship between review sentiment, title sentiment, stars, helpful etc??

roomba %>% 
  ggplot(aes(review_sentiment,stars)) +
  geom_point()
  

roomba %>% 
  ggplot(aes(review_sentiment,title_sentiment)) +
  geom_point() +
  

roomba %>% 
  ggplot(aes(review_sentiment,helpful)) +
  geom_point()  

## Rather than build a model, how to add a trend line? Or add stats to the graph?

## Search All Files for geam_smooth() and ggpubr to find the way!


## CHALLENGE: Grab a review for another product on amazon.co.uk - what kind of analysis can you do on it?

 ASIN <- "1491910399"
 
 r4ds <- c(1:13,15:30) %>% 
  map_dfr(~grab_amazon_full(ASIN, .x))

 ## Why not page 14? Try for your self to find out

# Bias towards 5 stars in helpful reviews?

 stars_count <-  
r4ds %>% filter(language == "eng") %>%
  count(stars)


r4ds %>% filter(language == "eng") %>%
  group_by(stars) %>% 
  summarise(helpful = sum(helpful)) %>% 
  inner_join(stars_count,.) %>% 
  pivot_longer(-stars) %>% 
  ggplot(aes(stars,value,group = name, color = name)) +
  geom_line() +
  labs(y = "count")
 

## How to find useful negative info?

## Use review_sentiment!

r4ds %>% filter(language == "eng") %>% 
  arrange(review_sentiment) %>% 
 pull(review) %>% 
  head(5)

## Use title_sentiment!

r4ds %>% filter(language == "eng") %>% 
  arrange(title_sentiment) %>% 
 pull(review) %>% 
  head(5)

## Use both!

r4ds %>% filter(language == "eng") %>%
  mutate(sentiment = review_sentiment + title_sentiment) %>% 
  arrange(sentiment) %>% 
 pull(review) %>% 
  head(5)
