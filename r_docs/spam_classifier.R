library(tidyverse)
library(rsample) # for splitting into training and test
library(tidytext) # for tokenization
library(janitor) # for tables
pacman::p_load_gh("timmarchand/keybindR") # for adding log odds  tables

spam <- read_csv("https://tinyurl.com/issSpam") # load data
write_csv(spam, "data/spam.csv") # save for later

glimpse(spam)


table(spam$label) # counts for spam vs ham
prop.table(table(spam$label)) # proportions for spam vs ham


## nice function for tables from janitor package
spam %>% 
  tabyl(label)


## What would be a good baseline value for predicting ham?
# The baseline is the accuracy of always predicting the majority class
spam %>% 
  tabyl(label) %>% 
  pull(percent) %>% 
  max()

## First step is to tokenize the text ----
## Here is a customize cleaner that can do the job,
## But often we'll use one from a package

string_cleaner <- function(text) {
  text %>%.  
    str_remove_all("[^[:alnum:] ]+") %>% #
    str_to_lower() %>% #
    str_replace_all("\\b(http|www).+?\\b", "_url_") %>% #
    str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>% #
    str_split_1(" ") %>% #
    keep(~ nchar(.x) > 0) #
}

msg <- "Check out http://example.com for details! Call 1234-567-890 if you have questions."
msg2 <- "!@##(@($#)@+$(#"

#' You can check out how the string_cleaner function works
#' by  un-commenting each step in the code below
#' To understand the last line, replace msg with msg2

msg %>% 
  
     #   str_remove_all( "[^[:alnum:] ]+") %>%
     #   str_to_lower() %>%
     #  str_replace_all("\\b(http|www).+?\\b", "_url_") %>%
     # str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>%
     #     str_split_1(" ") %>%
     #   keep(~ nchar(.x) > 0) %>%
  
  I() # Useful function when checking a pipe chain

## Check the function
string_cleaner(msg)

#' Alternative tokenizers ----

#' How are these different?

## tokenize with the same default as tidytext
tokenizers::tokenize_words(msg)

## tokenize using the penn tree bank format
tokenizers::tokenize_ptb(msg)

## Calculating log odds with bind_logOdds function ----

spam %>% 
   transmute(label, token = map(text, string_cleaner)) %>%
   unnest(cols = token) %>% 
   count(label, token) %>% 
   pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
   keybindR::bind_contingency_table("ham", "spam") %>% 
  I()

#' a = Count of the token in the target corpus.
#' b = Count of the token in the reference corpus.
#' c = Total count of tokens in the target corpus minus a (i.e., token absent in target corpus).
#' d = Total count of tokens in the reference corpus minus b (i.e., token absent in reference corpus).
#' n1 = Total count of tokens in the target corpus.
#' n2 = Total count of tokens in the reference corpus.
#' N = Total count of tokens in both corpora (n1 + n2).

spam %>% 
  transmute(label, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>% 
  count(label, token) %>% 
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  keybindR::bind_contingency_table("ham", "spam") %>% 
  bind_logOdds() %>% 
  remove_contingency_table() %>% # no longer needed
  I()

## From the bind_logOdds function ~ add a minimum amount to avoid / 0
# a_adj <- ifelse(tbl$a == 0, 0.5, tbl$a)
#  b_adj <- ifelse(tbl$b == 0, 0.5, tbl$b)
#  c_adj <- ifelse(tbl$c == 0, 0.5, tbl$c)
#  d_adj <- ifelse(tbl$d == 0, 0.5, tbl$d)
#  log_odds <- log((a_adj * d_adj)/(b_adj * c_adj))


## Separate the data into train and test ----

# make sure to do stratified sampling
split <- rsample::initial_split(spam, strata = label, prop = 0.8)
train <- rsample::training(split)
test <- rsample::testing(split)

## We'll use our custom tokenizer for now
train_scores <- train %>% 
  transmute(label, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>%
  # unnest_tokens("token", text) %>% 
  count(label, token) %>% 
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  bind_logOdds("ham","spam") %>% # note, creates contingency table if absent
  remove_contingency_table() %>% 
  select(token, score = log_odds) 


## Summary of the model
model <- train_scores %>% 
  mutate(label = ifelse(score > 0, "ham", "spam")) %>% 
  rename(log_odds = score) %>% 
  arrange(-log_odds)
model

## Calculate baseline from training data
baseline_log_odds <- train %>% 
  count(label) %>% 
  summarise(log_odds = log(n[1] / n[2])) %>% 
  pull(log_odds)
baseline_log_odds

# Alternative: Use log(1) = 0 to represent no prior knowledge
baseline <- 0  # represents 50-50 odds (neither ham nor spam more likely)

## Fit the model to our test data set
test_fit <- test %>% 
  rowid_to_column(var = "case") %>% 
  transmute(label, case, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>%
  left_join(train_scores) %>%  # Match tokens to their trained scores
  mutate(score = replace_na(score, baseline)) %>%  # Unknown words get baseline score
  mutate(turn_score = sum(score), .by = case) %>%  # Sum log-odds for each message
  mutate(prediction = ifelse(turn_score > 0, "ham", "spam"))  # Positive sum = ham


## Check the confusion matrix
cm <- caret::confusionMatrix(
  factor(test_fit$label, levels = c("ham", "spam")),  # Truth
  factor(test_fit$prediction, levels = c("ham", "spam"))  # Prediction
)
cm

## Check terms of interest
tidy(cm) %>% 
  filter(term %in% c("accuracy", "precision", "recall", "f1"))

## Visualize top predictive words (optional)
model %>% 
  slice_max(abs(log_odds), n = 20) %>% 
  mutate(token = fct_reorder(token, log_odds)) %>% 
  ggplot(aes(log_odds, token, fill = label)) +
  geom_col() +
  labs(title = "Most Distinctive Words for Classification",
       x = "Log-Odds (← Spam | Ham →)",
       y = NULL) +
  theme_minimal()

## classify_text function

source("https://tinyurl.com/classifyText")

classify_text

## Use classify_text function on new input
# This function takes a text string and classifies it using our trained model
classify_text(msg, 
              model = model, # specify model
              category = "label", # specify the category col
              tokenizer = "clean") # specify tokenizer


## For more details, add show_scores = TRUE
classify_text(msg, 
              model = model, 
              category = "label", 
              show_scores = TRUE, # see the word scores in context
              tokenizer = "clean") # tidytext is default)

## What happens when we change the tokenizer?

result <- classify_text(msg, 
                        model = model, 
                        category = "label", 
                        show_scores = TRUE,
                        tokenizer = "tidy") 

result[[1]] # same as result$classification
result[[2]] # same as result$total_score
result[[3]] # same as result$token_scores


## What does this show?
result[[3]] %>%
  enframe(name = "token", value = "score") %>% 
  summarise(sum = sum(score))
