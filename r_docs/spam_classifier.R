library(tidyverse)
library(rsample)
library(tidytext)
library(keybindR)

spam <- read_csv("data/spam.csv")

glimpse(spam)

names(spam) <- c("label", "text")
table(spam$label) # counts for spam vs ham
prop.table(table(spam$label)) # proportions for spam vs ham

## What would be the baseline for a classification model?
# prop.table(table(spam$label)) %>% (function(x) x[1] / x[2]) %>% log

## First step is to tokenize the text ----
## Here is a customize cleaner that can do the job,
## But often we'll use one from a package

string_cleaner <- function(text) {
  text %>%
    str_remove_all("[^[:alnum:] ]+") %>%
    str_to_lower() %>%
 str_replace_all("\\b(http|www).+?\\b", "_url_") %>% 
  str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>% 
      str_split_1(" ") %>%
    keep(~ nchar(.x) > 0)
}

msg <- "Check out http://example.com for details! Call 1234-567-890 if you have questions."
msg2 <- "!@##(@($#)@+$(#"

#' You can check out how the string_cleaner function works
#' by  un-commenting each step in the code below
#' To understand the last line, replace msg with msg2

msg %>% 
  
#     str_remove_all( "[^[:alnum:] ]+") %>%
#     str_to_lower() %>%
#    str_replace_all("\\b(http|www).+?\\b", "_url_") %>%
#   str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>%
#       str_split_1(" ") %>%
#     keep(~ nchar(.x) > 0) %>%
  
I() # Useful function when checking a pipe chain

## Check the function
clean_string(msg)

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


## Fit the model to our test data set
test_fit <- test %>% 
  rowid_to_column(var = "case") %>% 
  transmute(label, case, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>%
  left_join(train_scores) %>% 
  mutate(score = replace_na(score, log(baseline))) %>% 
  mutate(turn_score = sum(score), .by = case) %>% 
  mutate(prediction = ifelse(turn_score > 0, "ham", "spam"))


## Check the confusion matrix
cm <- caret::confusionMatrix(
  factor(test_fit$label, levels = c("ham", "spam")),  # Truth
  factor(test_fit$prediction, levels = c("ham", "spam"))  # Prediction
)

## Check terms of interest
tidy(cm) %>% 
     filter(term %in% c("accuracy", "precision", "recall", "f1"))


## Use classify_text function on new input
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

# result[[3]] %>% 
#   enframe(name = "token", value = "score")

      