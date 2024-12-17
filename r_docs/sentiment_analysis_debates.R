## Load the libraries

library(tidyverse)
library(dtagger)
library(keybindR)
library(tidytext)
library(plotly)
library(syuzhet) # for sentiment analysis
pacman::p_load(gt) # for html tables in viewer

dbts <- read_csv("https://docs.google.com/spreadsheets/d/1L7vlB2fI87y1llURT6e4cxJObWj_teVQJvvYi2ExEAI/export?format=csv&gid=735200680")

## save to data folder
write_csv(dbts, "data/all_debates.csv")

## glimpse
glimpse(dbts)


## load helper functions from source

source("r_docs/debate_helpers.R")


## Make some comparisons

Clintons <- classify_debaters(dbts, c("CLINTON_H","CLINTON_W"))

Clintons [[1]] # table of of text data filtered by speaker
Clintons [[2]] # confusion matrix result; can be input for tidy_cm()
Clintons [[3]] # model results with token, log_odds and predictions
Clintons [[4]] # top n features side-by-side
Clintons [[5]] # top n features in one graph
Clintons [[6]] # word count vs test accuracy

BT_20 <- classify_debaters(dbts, c("BIDEN","TRUMP"), 
                           year_range = 2020, 
                           top_n_features = 10 )

BT_20$text_data # same as BT_20 [[1]]
BT_20$confusion_matrix %>% tidy_cm() # same as BT_20 [[2]] %>% tidy_cm
BT_20$model_scores # same as BT_20 [[3]]
BT_20$top_features_plots # same as BT_20 [[4]]
BT_20$top_features_comparison # same as BT_20 [[5]]
BT_20$accuracy_plot # same as BT_20 [[6]]

NT <- classify_debaters(dbts, c("NIXON","TRUMP"),
                        sample_prop = 0.8) # change training:testing to 80:20

NT [[4]]

NT [[1]] %>% 
  generate_kwic(keyword = "\\b[Mm]r") %>% 
  gt::gt()


NT [[1]] %>% 
  generate_kwic(keyword = "\\b[Ss]he\\b") %>% 
  gt::gt()


NT [[1]] %>% 
  generate_kwic(keyword = "\\b[Ll]ot\\b") %>% 
  gt::gt()


## sentiment analysis ----

## subset for the Harris-Trunp debate in 2024
HT <- dbts %>% 
  filter(str_detect(debate_id, "HT"))


## sentiment scores with syuhet

p <- HT %>% 
  mutate(sentiment = get_sentiment(text)) %>% 
  filter(speaker %in% c("TRUMP", "HARRIS")) %>% 
  ggplot() +
  aes(x = turn, y = sentiment, color = speaker) +
  geom_line() +
  scale_color_manual(values = c("#0033cc","#d30b0d"))


p

plotly::ggplotly(p)

## check turns

HT %>% 
  filter(turn == 159) %>% 
  pull(text)

HT %>% 
  filter(turn == 107) %>% 
  pull(text)

HT_sentence_level <-  HT %>% 
  unnest_sentences("sentence", text) %>% 
  mutate(sentence_n = row_number(),
         sentiment = get_sentiment(sentence)) %>% 
   filter(speaker %in% c("TRUMP", "HARRIS")) 

p2 <- HT_sentence_level %>% 
  ggplot() +
  aes(x = sentence_n, y = sentiment, color = speaker) +
  geom_line() +
  scale_color_manual(values = c("#0033cc","#d30b0d"))


ggplotly(p2)

HT_sentence_level %>% 
  filter(sentence_n == 1434) %>% 
  pull(sentence)

HT_sentence_level %>% 
  filter(sentence_n == 283) %>% 
  pull(sentence)

## using the lexicons from tidytext ----

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc") ## you may have to download it


## match all nrc sentiments
HT_nrc <- HT %>% 
    filter(speaker %in% c("TRUMP", "HARRIS")) %>% 
  unnest_tokens("word", text) %>% 
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") 

# check
HT_nrc

# count sentiments by speaker
HT_nrc %>% 
  count(speaker, sentiment, sort = TRUE)

## Most common positive words
HT_nrc %>% 
  filter(sentiment == "positive") %>% 
  count(speaker, word, sort = TRUE)

## Most common negative words
HT_nrc %>% 
  filter(sentiment == "negative") %>% 
  count(speaker, word, sort = TRUE)

## Show top anger words

HT_nrc %>% 
  filter(sentiment %in% c("anger")) %>% 
  count(speaker,sentiment, word) %>% 
   group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot() +
  aes(y = reorder_within(word,n, speaker), x = n, fill = speaker) +
  geom_col() +
  facet_wrap(~speaker, scales   = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c("#0033cc","#d30b0d")) +
  labs(y = "word",
       title = "Angry words in Trump-Harris debate")

## Show top joy words
HT_nrc %>% 
  filter(sentiment %in% c("joy")) %>% 
  count(speaker,sentiment, word) %>% 
   group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
#  mutate(word = fct_reorder(word, n)) %>% 
  ggplot() +
  aes(y = reorder_within(word,n, speaker), x = n, fill = speaker) +
  geom_col() +
  facet_wrap(~speaker, scales   = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c("#0033cc","#d30b0d")) +
  labs(y = "word",
       title = "Joyful words in Trump-Harris debate")

## Check the use of "baby"
HT %>% 
  generate_kwic(keyword = "[Bb]aby") %>% 
  gt::gt()
