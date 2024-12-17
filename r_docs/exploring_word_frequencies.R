## Word frequencies in text
## Load libraries
pacman::p_load(remotes, # for installing from github
               tidytext,
               quanteda,
               quanteda.textplots)
remotes::install_github("lchiffon/wordcloud2")
remotes::install_github("timmarchand/dtagger")
library(wordcloud2)
library(tidyverse)

## Choose the theme for all the plots
theme_set(theme_minimal())


HT <- read_csv("data/HT_dbt.csv") %>% 
  rowid_to_column(var = "turn") 
#  filter(speaker %in% c("TRUMP", "HARRIS")) 

## Choose the stop_words
stopwords <- stop_words %>% 
  filter(lexicon == "snowball")

## tokenize (tidy) ----
## With or without stopwords
HT %>% 
  unnest_tokens("word", text) %>% 
#  anti_join(stopwords)%>% 
  count(word, sort = TRUE) %>% 
  I()

## Simple column display plots ----


HT_count <- HT %>% 
  unnest_tokens("word", text) %>% 
  # anti_join(stopwords)%>% 
  count(word, sort = TRUE) 
  

HT_count %>% 
  anti_join(stopwords) %>% 
  slice_max(n, n = 20) %>% 
  ggplot()+
  aes(n, reorder(word, n)) +
  geom_col() +
  labs(x = "count", y = "word")


## Separate by speaker and add proportion column
HT_prop <- HT %>% 
  unnest_tokens("word", text) %>% 
  # anti_join(stopwords)%>% 
  count(speaker, word, sort = TRUE) %>% 
  mutate(proportion = n / sum(n))

HT_prop %>% 
  anti_join(stopwords)%>% 
  filter(speaker %in% c("TRUMP", "HARRIS")) %>% 
  group_by(speaker) %>% 
  slice_max(n, n = 20) %>% 
  ggplot()+
  aes(x = n, y = reorder_within(word, n,  speaker), fill = speaker) +
  geom_col()+
  facet_wrap(~speaker, scales = "free")+
  scale_y_reordered() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "word")


## Wordclouds with wordcloud2 ----
library(wordcloud2)

wc1 <- HT %>% 
  unnest_tokens("word", text) %>% 
  anti_join(stopwords) %>% 
  count(speaker, word, sort = TRUE) %>%
  filter(speaker == "TRUMP") %>% 
select(word, freq = n) %>% 
  wordcloud2(size = 0.5) # Change the size of the font to fit most freq word

wc1

wc2 <- HT %>% 
  unnest_tokens("word", text) %>% 
  anti_join(stopwords) %>% 
  count(speaker, word, sort = TRUE) %>%
  filter(speaker == "HARRIS") %>% 
select(word, freq = n) %>% 
  wordcloud2(size = 0.5) # Change the size of the font to fit most freq word


wc2

## Wordclouds with quanteda.textplots ----


## The document-feature-matrix (document-term-matrix)
HT_dfm <-  corpus(HT) |>
    tokens(remove_punct = TRUE) |>
    tokens_remove(pattern = stopwords('english')) |>
    dfm() |>
    dfm_trim(min_termfreq = 10, verbose = FALSE)

HT_dfm

docvars(HT_dfm)

HT_dfm %>% 
  broom::tidy()


HT_dfm <-  corpus(HT) |>
    tokens(remove_punct = TRUE) |>
    tokens_remove(pattern = stopwords('english')) |>
    dfm() 


HT_dfm|>
    dfm_trim(min_termfreq = 10, verbose = FALSE)|>
    textplot_wordcloud(HT_dfm)


HT_dfm
    dfm_group(groups = speaker) |>
    dfm_trim(min_termfreq = 5, verbose = FALSE) |>
    textplot_wordcloud(comparison = TRUE )


## Multiple speakers
    HT <- read_csv("data/HT_dbt.csv") %>% 
  rowid_to_column(var = "turn") 
    
corpus(HT) |>
#corpus_subset(speaker %in% c("MUIR", "TRUMP", "HARRIS")) |>
  tokens(remove_punct = TRUE) |>
    tokens_remove(stopwords("english")) |>
    dfm() |>
    dfm_group(groups = speaker) |>
    dfm_trim(min_termfreq = 5, verbose = FALSE) |>
    textplot_wordcloud(comparison = TRUE )


# How are the words used? KWIC concordance lines ----

# KWIC stands for Key Word in Context
# Often used to understand how words are used by different people


library(dtagger) # for quick concordancing with quick_conc
?quick_conc
#' x =	a character vector of tokenized strings, or a single string
#' index =	a character vector of regex pattern to match, or a numeric vector to use as index of matches
#' n	= an integer, to specify the number of context tokens either side of the matched node
#' tokenize	= a logical, to tokenize the text first or not. If TRUE, a very basic tokenizer is used to split the string on whitespaces and punctuation (but not word internal apostrophes, at marks and hyphens).
#' separated	= a logical, to separate the context tokens or not


quick_conc(HT$text,index =  "people", tokenize = TRUE) %>% 
  View

## add regex for word breaks to isolate individual words
quick_conc(HT$text,index =  "America", tokenize = TRUE) %>% 
  View
quick_conc(HT$text,index =  "\\bAmerica\\b", tokenize = TRUE) %>% 
  View


## use map with the tibble to maintain speaker information

HT %>% 
  mutate(tokens = map(text, ~.x %>% tokenize_words %>% unlist)) %>% 
  mutate(kwic = map(tokens, ~quick_conc(.x, index =  "people"))) %>% 
  unnest(kwic) %>% 
  select(speaker, left:right)


## Zipf's law ----

alpha <- 1

HT %>% 
  unnest_tokens("word", text) %>% 
 # anti_join(stopwords) %>% 
  count(word, sort = TRUE) %>%
  rowid_to_column(var = "rank") %>% 
  mutate(zipf_n = first(n) / rank^alpha) %>% 
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(rank, n)) +
  geom_line(color = "blue") +
  geom_line(aes(rank, zipf_n), color = "red")+
 # coord_trans(y = "log10", x = "log10") +
  labs(title = "Rank Frequency Plot", x = "Rank", y = "Frequency")
