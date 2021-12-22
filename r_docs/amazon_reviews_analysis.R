## load libraries
pacman::p_load(tidyverse,
               rvest,
               franc, # we will use this to detect the language of reviews
               sentimentr, # for polarity of opinion analysis
               lexicon) # for information about words

source("r_docs/grab_amazon_full.R")
source("r_docs/see_regex.R")



## web-scraping amazon review data ----
## Check out the example for playstation 4
#https://www.amazon.co.uk/product-reviews/B07HSJW7HK/

## Review for ps4
ASIN <- "B07HSJW7HK"
  
ps4 <- 1:63 %>% 
  map_dfr(~grab_amazon_full(ASIN, .x))

ps4 %>% select(-title_sent, - review_sent)

## simple wordcounts ----
## grab a sample to experiment on
sample_reviews <- ps4 %>% 
  select(review) %>% 
  sample_n(20) %>% 
  pull()

sample_reviews

## How to count words?
## Here are some possible regex we can use
## Try them out using see() on the sample_reviews, try and spot the differences


## REGEX

# "[:alpha:]"
# "[a-z]+"
# "[A-z]"
# "\\w"
# "\\w+"
# "\\S+"
# "[^ ]+"



see("[a-z]+", string = sample_reviews)


## Use REGEX to count words defined as any sequence of characters which are not spaces
str_count(sample_reviews, "____")

## apply to the df
ps4 <- ps4 %>% mutate(wc = str_count(review,"____"))


## find the correlation between review_wc and wc
## if you get 1.00, you found the same regex!
ps4 %>% 
  summarise(correl = cor(review_wc, wc))


## Reviews in English ----

ps4_english <- ps4 %>% filter(language =="eng")
ps4_english 
## How did we detect the English reviews?
## Using the function franc::franc() which looks at individual words, and compares them to an internal dictionary
## This requires a single length character string input to work


 franc::franc(sample_reviews)
 # Error in franc_all(text, min_speakers = min_speakers, whitelist = whitelist,  : 
 #                      length(text) == 1 is not TRUE
 
## In this case, we can use purr::map function to iterate over the vector one at a time
 map(sample_reviews,franc) # returns a list
 ## To return a character vector, use map_chr
 map_chr(sample_reviews,franc)
 
 ## You can see the list of languages with the speakers dataframe that comes with the package:
 head(speakers)
 
 ## Let's add the language name to our dataframe
 
 ## How can we add this data?
 speakers %>% 
   select(language, language_name = name) %>% 
   tibble()  # add a pipe and a line of code below
 
 
 # sentiment analysis ----
 ## let's grab a new sample, this time from English reviews only
 eng_reviews <- 
   ps4 %>% filter(language == 'eng') %>% 
   sample_n(5) %>% 
   select(review, review_sentiment) %>% 
   mutate(id = row_number())
 
eng_reviews %>% select(id,review)
 
## Challenge read the reviews - rank them from most positive to most negative by id number below

## most positive
## 
## 
## 
## 
## 
## most negative


## How close was your match?
eng_reviews %>% arrange(-review_sentiment)


## How does it work?
## Words are assigned sentiment "scores" based on this list
sentiment_df <- 
lexicon::hash_sentiment_jockers_rinker %>% 
  tibble()


## Find some of the words from the eng_reviews to see their scores
## Note they need to be in lower case
sentiment_df %>% 
  filter(x == "works")

## Can you think of any problems with this simple approach??

## Valence shifters ----

valence_df <- 
  lexicon::hash_valence_shifters %>% 
  tibble()


valence_shifter_key <- tibble(meaning = c("negator", "intensifier", "downtoner", "adversative_conjuctive"))


## Challenge have a look at the different words associated with the y values
## Complete the valence_shifter_key with values 1~4

valence_df %>% filter(y == 1)

valence_shifter_key %>% 
  mutate(y = case_when(meaning == "adversative_conjuctive" ~ ___,
                        meaning == "downtoner" ~ ___,
                        meaning == "intensifier" ~ ___,
                        meaning == "negator" ~ ___))

 
 
## Let"s try a new review, for the roomba this time ----
 ## https://www.amazon.co.uk/iRobot-ROOMBA980-Robot-Vacuum-Cleaner/dp/B013E9L4ZS

 roomba <- 
   1:50 %>% 
   map_dfr(~grab_amazon_full(ASIN, .x))
 
roomba


## CHALENGES ----


## CHALENGE: How accurately did the franc::franc identify languages? Find out how many reviews in English were misidentifed

## CHALLENGE: Grab a review for another product on amazon.co.uk - what kind of analysis can you do on it?

 
## CHALLENGE: Is there a correlation between word count and whether the review is considered helpful (for either ps4 or roomba or another product)?
## CHALLENGE: Does language of review have an effect on how helpful the review is considered (for either ps4 or roomba or another product)?
## CHALLENGE: Is there an effect of the rating score on how helpful the review is considered (for either ps4 or roomba or another product)?
## CHALLENGE: Can you visualise a trend over time for the ratings (for either ps4 or roomba or another product)?
## CHALLENGE: What is the relationship between review sentiment, title sentiment, rating, helpful etc??
