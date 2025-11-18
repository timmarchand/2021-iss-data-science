# Text Classification with Log Odds: Spam Detection
library(tidyverse)
library(rsample) # for splitting into training and test
library(tidytext) # for tokenization
library(janitor) # for tables
library(caret) # for confusion matrix
pacman::p_load_gh("timmarchand/keybindR") # for adding log odds  tables


## Learning Objectives ----

# By the end of this session, you will be able to:
# 1. Explain how log odds can be used to identify characteristic words in text
# 2. Build a text classifier using log odds ratios (a Naive Bayes approach)
# 3. Understand the connection between logistic regression and log odds text classification
# 4. Evaluate text classifiers using confusion matrices and performance metrics
# 5. Compare different tokenization approaches and their effects on classification

# -----------------------------------------------------------------------------

## The Big Picture: From Logistic Regression to Text Classification ----

# In logistic regression, we learned:
# - Log odds are useful for modeling binary outcomes
# - We use ONE predictor (or a few) to predict probability
#
# For text classification, we extend this idea:
# - Each WORD contributes its own log odds to the classification
# - We SUM the log odds across all words in the document
# - This is based on the Naive Bayes assumption: words are independent
#
# Mathematical connection:
# - Logistic regression: log(p/(1-p)) = β₀ + β₁X
# - Text classification: log(p/(1-p)) = Σ(log odds for each word)
#
# Key insight: Words that appear more often in HAM than spam have POSITIVE log odds
#             Words that appear more often in SPAM than ham have NEGATIVE log odds
#             We sum these up to get an overall classification!
#             (Because we specified "ham" as target corpus in bind_logOdds)

# -----------------------------------------------------------------------------

## Load and Explore the Data ----

spam <- read_csv("https://tinyurl.com/issSpam") # load data
write_csv(spam, "data/spam.csv") # save for later

glimpse(spam)



# Check the class distribution
table(spam$label) # counts for spam vs ham
prop.table(table(spam$label)) # proportions for spam vs ham


## nice function for tables from janitor package
spam %>% 
  tabyl(label)

# BASELINE ACCURACY: What if we just predicted everything as "ham"?
baseline_accuracy <- tabyl(spam, label) %>% 
  pull(percent) %>% 
  max
cat("\nBaseline accuracy (always predict 'ham'):", 
    round(baseline_accuracy * 100, 1), "%\n")
cat("Our goal: Build a classifier that beats", round(baseline_accuracy * 100, 1), "%!\n\n")

# -----------------------------------------------------------------------------

## Understanding Tokenization ----

# Before we can analyze text, we need to break it into individual words (tokens)
# Different tokenizers handle punctuation, URLs, and numbers differently

# Let's compare three approaches:

msg <- "Check out http://example.com for details! Call 1234-567-890 if you have questions."
msg2 <- "!@##(@($#)@+$(#"

### Option 1: Custom string cleaner (we built this ourselves!)
string_cleaner <- function(text) {
  text %>%
    str_remove_all("[^[:alnum:] ]+") %>%           # Remove non-alphanumeric except spaces
    str_to_lower() %>%                              # Convert to lowercase
    str_replace_all("\\b(http|www).+?\\b", "_url_") %>%  # Replace URLs with _url_
    str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>%  # Replace long numbers
    str_split_1(" ") %>%                            # Split into words
    keep(~ nchar(.x) > 0)                           # Remove empty strings
}

# TASK 1: Uncomment each line below ONE AT A TIME to see what each step does:
msg %>% 
  #   str_remove_all("[^[:alnum:] ]+") %>%
  #   str_to_lower() %>%
  #   str_replace_all("\\b(http|www).+?\\b", "_url_") %>%
  #   str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>%
  #   str_split_1(" ") %>%
  #   keep(~ nchar(.x) > 0) %>%
  I() # I() is a useful function for inspecting a pipe chain - it returns the input unchanged

# Test the complete function:
string_cleaner(msg)

### Option 2: tidytext default tokenizer
tokenizers::tokenize_words(msg)
# Notice: removes punctuation, keeps URL as-is, splits phone number

### Option 3: Penn Tree Bank tokenizer (linguistic standard)
tokenizers::tokenize_ptb(msg)
# Notice: keeps more punctuation, treats URLs differently

# For spam detection, we'll use our custom tokenizer because:
# - URLs and phone numbers are VERY important spam indicators
# - We want to treat them as special tokens (_url_, _longnum_)
# - This gives our classifier more signal to work with

# -----------------------------------------------------------------------------

## Building the Model: Computing Log Odds for Each Word ----

# The key idea: Compare word frequencies in spam vs. ham
# Words common in spam but rare in ham get HIGH log odds
# Words common in ham but rare in spam get LOW (negative) log odds

# Step 1: Create the contingency table
# For each word, we need to know:
# - How many times it appears in spam (a)
# - How many times it appears in ham (b)
# - How many OTHER words appear in spam (c = total_spam - a)
# - How many OTHER words appear in ham (d = total_ham - b)

## Uncomment each line in the pipe again to understand the steps
spam %>% 
  # transmute(label, token = map(text, string_cleaner)) %>%
  # unnest(cols = token) %>% 
  # count(label, token) %>% 
  # pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  # keybindR::bind_contingency_table("ham", "spam") %>% 
  I()


# Let's see this in action:
word_comparison <- spam %>% 
  transmute(label, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>% 
  count(label, token) %>% 
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  keybindR::bind_contingency_table("ham", "spam") 

# Look at the structure:
word_comparison %>% 
  select(token, ham, spam, a, b, c, d, n1, n2, N) %>% 
  sample_n(n = 10)

# Interpretation of contingency table columns:
# ham, spam = raw counts of this word in each corpus
# a = count in target corpus (spam)
# b = count in reference corpus (ham)  
# c = all OTHER words in spam corpus
# d = all OTHER words in ham corpus
# n1, n2 = total word counts in each corpus
# N = grand total of all words

# Step 2: Calculate log odds for each word

## Uncomment each line in the pipe again to understand the steps
word_comparison %>% 
  keybindR::bind_logOdds() %>%   # This computes: log((a*d)/(b*c))
  remove_contingency_table() %>%  # Clean up - we don't need a,b,c,d anymore
  select(token, score = log_odds) %>% 
  I()  # Pause here to see the log odds scores

word_scores <- word_comparison %>% 
  keybindR::bind_logOdds() %>%   # This computes: log((a*d)/(b*c))
  remove_contingency_table() %>%  # Clean up - we don't need a,b,c,d anymore
  select(token, score = log_odds)

# Let's look at the most "spammy" and "hammy" words:
word_scores %>% 
  arrange(desc(score)) %>% 
  head(20)  # Positive scores = ham indicators (target corpus)

word_scores %>% 
  arrange(score) %>% 
  head(20)  # Negative scores = spam indicators (reference corpus)

# TASK 2: Discuss with a partner:
# - Do these word rankings make intuitive sense?
# - Which words surprise you?
# - Why might some common words have extreme scores?

# -----------------------------------------------------------------------------

## Train/Test Split ----

# Just like with logistic regression, we need to:
# 1. Train on one subset of data
# 2. Test on a completely separate subset
# This tells us if our classifier generalizes to new messages!

set.seed(42)
split <- rsample::initial_split(spam, strata = label, prop = 0.8)
train <- rsample::training(split)
test <- rsample::testing(split)

cat("Training set:", nrow(train), "messages\n")
cat("Test set:", nrow(test), "messages\n")

# -----------------------------------------------------------------------------

## Training the Classifier ----

# Compute word scores using ONLY the training data
# (We can't peek at the test set!)

train_scores <- train %>% 
  transmute(label, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>%
  count(label, token) %>% 
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  bind_logOdds("ham","spam") %>%  # Creates contingency table automatically
  remove_contingency_table() %>% 
  select(token, score = log_odds)

# Our "model" is just this lookup table of word scores!
# Let's examine it:
model <- train_scores %>% 
  mutate(label = ifelse(score > 0, "ham", "spam")) %>%  # Positive scores = ham (target corpus)
  rename(log_odds = score) %>% 
  arrange(desc(log_odds))

head(model, 20)  # Top ham indicators (positive log odds)
tail(model, 20)  # Top spam indicators (negative log odds)

# Visualize the most distinctive words:
model %>% 
  group_by(label) %>% 
  slice_max(abs(log_odds), n = 10) %>% 
  mutate(token = fct_reorder(token, -log_odds)) %>% 
  ggplot(aes(log_odds, token, fill = label)) +
  geom_col() +
  labs(title = "Most Distinctive Words for Classification",
       subtitle = "Words that strongly indicate ham (positive) or spam (negative)",
       x = "Log-Odds (← Ham | Spam →)",
       y = NULL) +
  scale_fill_manual(values = c("ham" = "cyan4", "spam" = "darkorange")) +
  theme_minimal()

# What does this visualization show?
# - Words on the right (positive log odds) = strong ham indicators
# - Words on the left (negative log odds) = strong spam indicators  
# - The further from zero, the stronger the signal

# -----------------------------------------------------------------------------

## Making Predictions on Test Data ----

# For each test message:
# 1. Break it into words (tokens)
# 2. Look up the log odds for each word
# 3. SUM all the log odds
# 4. If sum > 0, predict HAM; if sum < 0, predict SPAM

# Calculate default for unknown words
# (If we see a word not in our training data, what score should we give it?)
default <- log(sum(train$label == "ham") / sum(train$label == "spam"))
default2 <- 0 # no prior assumption of word "spamminess" (log odds = 0 = 50/50 chance)
cat("Default log odds for unknown words:", round(baseline, 3), "\n")

test_fit <- test %>% 
  rowid_to_column(var = "case") %>% 
  transmute(label, case, token = map(text, string_cleaner)) %>%
  unnest(cols = token) %>%
  left_join(train_scores, by = "token") %>% 
  mutate(score = replace_na(score, default)) %>%  # Unknown words get baseline score
  summarise(turn_score = sum(score), 
            label = first(label),
            .by = case) %>% 
  mutate(prediction = ifelse(turn_score > 0, "ham", "spam"))

# Let's look at some predictions:
test_fit %>% 
  select(case, label, turn_score, prediction) %>% 
  head(10)

# Interpretation:
# - Positive turn_score = model thinks it's ham (target corpus)
# - Negative turn_score = model thinks it's spam (reference corpus)
# - Larger absolute values = more confident predictions

# -----------------------------------------------------------------------------

## Evaluating the Classifier ----

# Confusion Matrix: Compare our predictions to the truth
cm <- caret::confusionMatrix(
  factor(test_fit$label, levels = c("ham", "spam")),       # Truth
  factor(test_fit$prediction, levels = c("ham", "spam"))   # Prediction
)

cm

# Extract key metrics
metrics <- broom::tidy(cm) %>% 
  filter(term %in% c("accuracy", "precision", "recall", "f1", "kappa"))

metrics

# Compare to baseline:
cat("\n=== PERFORMANCE COMPARISON ===\n")
cat("Baseline (always predict 'ham'):", round(baseline_accuracy * 100, 1), "%\n")
cat("Our classifier accuracy:        ", round(metrics$estimate[metrics$term == "accuracy"] * 100, 1), "%\n")
cat("Improvement:                    +", 
    round((metrics$estimate[metrics$term == "accuracy"] - baseline_accuracy) * 100, 1), 
    "percentage points\n\n")

# Interpreting the metrics:
cat("=== METRIC INTERPRETATION ===\n")
cat("Accuracy:  ", round(metrics$estimate[metrics$term == "accuracy"], 3), 
    "- Overall correct predictions\n")
cat("Precision: ", round(metrics$estimate[metrics$term == "precision"], 3), 
    "- Of predicted spam, how much is actually spam?\n")
cat("Recall:    ", round(metrics$estimate[metrics$term == "recall"], 3), 
    "- Of actual spam, how much did we catch?\n")
cat("F1:        ", round(metrics$estimate[metrics$term == "f1"], 3), 
    "- Balance between precision and recall\n")
cat("Kappa:     ", round(metrics$estimate[metrics$term == "kappa"], 3), 
    "- Agreement beyond chance\n\n")

# TASK 3: Which is more important for spam filtering - precision or recall?
# Discuss: Would you rather miss some spam (low recall) or 
#          have some ham marked as spam (low precision)?

# -----------------------------------------------------------------------------

## Using the Classifier on New Messages ----

# We'll use a classify_text() function that makes it easy to apply our trained 
# model to new messages. For now, we'll source it from a URL.
# (In the future, this will be part of the keybindR package)

source("https://tinyurl.com/classifyText")

# How classify_text() works:
# 1. Tokenizes the input text using your chosen method
# 2. Looks up each token's log odds score in the model
# 3. Assigns 0 to any tokens not in the model (unknown words)
# 4. Sums all the token scores to get a total score
# 5. If total > 0, predicts the first category; if total < 0, predicts the second
#
# Function signature:
# classify_text(text, model, show_scores = FALSE, category = "label", tokenizer = "tidy")

# Test message 1: Obvious spam
test_msg1 <- "Call now to claim your FREE prize! Click here: http://scam.com"

result1 <- classify_text(test_msg1, 
                         model = model,
                         category = "label",
                         tokenizer = "clean")  # Uses string_cleaner function
result1


# Test message 2: Obvious ham (normal message)
test_msg2 <- "Hi, are we still meeting for coffee tomorrow afternoon?"

result2 <- classify_text(test_msg2,
                         model = model,
                         category = "label", 
                         tokenizer = "clean")
result2


# -----------------------------------------------------------------------------

## Understanding Individual Word Contributions ----

# Set show_scores = TRUE to see which words drove the classification
# This returns a list with three components:
# 1. classification - the predicted category
# 2. total_score - the sum of all word scores
# 3. token_scores - a named vector showing each word's contribution

result1_detailed <- classify_text(test_msg1, 
                                  model = model, 
                                  category = "label", 
                                  show_scores = TRUE,
                                  tokenizer = "clean")


result1_detailed[[1]] # same as result1_detailed$classification
result1_detailed[[2]] # same as result1_detailed$total_score
result1_detailed[[3]] # same as result1_detailed$token_scores


## What does this show?
result1_detailed[[3]] %>%
  enframe(name = "token", value = "score") %>% 
  summarise(sum = sum(score))
# Examine the components:
cat("=== DETAILED CLASSIFICATION BREAKDOWN ===\n\n")

cat("1. CLASSIFICATION:", result1_detailed$classification, "\n\n")

cat("2. TOTAL SCORE:", round(result1_detailed$total_score, 2), "\n")
cat("   (Positive = spam, Negative = ham)\n\n")

cat("3. INDIVIDUAL WORD SCORES:\n")
# Convert to a tibble for better display
token_contributions <- tibble(
  token = names(result1_detailed$token_scores),
  log_odds = result1_detailed$token_scores
) %>% 
  arrange(desc(log_odds))

print(token_contributions, n = Inf)

# Which words pushed it toward spam?
cat("\n=== SPAM INDICATORS (positive scores) ===\n")
token_contributions %>% 
  filter(log_odds > 0) %>% 
  print(n = Inf)

# Which words pushed it toward ham?
cat("\n=== HAM INDICATORS (negative scores) ===\n")
token_contributions %>% 
  filter(log_odds < 0) %>% 
  print(n = Inf)

# Words not in our training data get a score of 0
cat("\n=== UNKNOWN WORDS (score = 0) ===\n")
token_contributions %>% 
  filter(log_odds == 0) %>% 
  print(n = Inf)

# Key insight: The classification is determined by which type of words
# dominate in the message. Even one or two strong spam indicators
# (like "free", "_url_") can outweigh several neutral or ham words.

# -----------------------------------------------------------------------------

## Interactive Testing ----

# TASK 4: Test the classifier with your own messages!

# Example 1: Borderline case - legitimate promotional email
test_msg3 <- "Thank you for subscribing to our newsletter. You can unsubscribe anytime."

result3 <- classify_text(test_msg3,
                         model = model,
                         category = "label",
                         show_scores = TRUE,
                         tokenizer = "clean")

cat("\n=== YOUR TEST MESSAGE ===\n")
cat("Text:", test_msg3, "\n")
cat("Prediction:", result3$classification, "\n")
cat("Total score:", round(result3$total_score, 2), "\n\n")

# Example 2: Try to fool the classifier!
# Can you write a spam message that gets classified as ham?
# Or a ham message that gets classified as spam?

# Your turn - write your test messages here:
my_test_message <- "Your message here..."

# Uncomment to test:
# my_result <- classify_text(my_test_message,
#                            model = model,
#                            category = "label",
#                            show_scores = TRUE,
#                            tokenizer = "clean")
# 
# cat("Prediction:", my_result$classification, "\n")
# cat("Total score:", round(my_result$total_score, 2), "\n")

# Questions to explore:
# - What makes a message "spammy" to the classifier?
# - Can you find words that strongly indicate spam or ham?
# - How does the classifier handle mixed signals (both spam and ham words)?
# - What happens with very short messages vs. long messages?

# -----------------------------------------------------------------------------

## Comparing Tokenizers ----

# Earlier we saw three different tokenizers. How do they affect classification?

# Test with tidytext tokenizer (default)
result_tidy <- classify_text(test_msg1, 
                             model = model, 
                             category = "label", 
                             show_scores = TRUE,
                             tokenizer = "tidy")

# You can access result components by name OR by index:
result_tidy$classification  # OR result_tidy[[1]]
result_tidy$total_score     # OR result_tidy[[2]]
result_tidy$token_scores    # OR result_tidy[[3]]

# Compare: How many tokens did each tokenizer find?
result_tidy[[3]] %>%
  enframe(name = "token", value = "score") %>% 
  summarise(
    n_tokens = n(),
    total_score = sum(score)
  )

# Compare the results:
cat("\n=== TOKENIZER COMPARISON ===\n")
cat("Custom tokenizer:\n")
cat("  Classification:", result1_detailed$classification, "\n")
cat("  Total score:", round(result1_detailed$total_score, 2), "\n")
cat("  Number of tokens:", length(result1_detailed$token_scores), "\n")
cat("  Tokens:", paste(names(result1_detailed$token_scores), collapse = ", "), "\n\n")

cat("Tidytext tokenizer:\n")
cat("  Classification:", result_tidy$classification, "\n")
cat("  Total score:", round(result_tidy$total_score, 2), "\n")
cat("  Number of tokens:", length(result_tidy$token_scores), "\n")
cat("  Tokens:", paste(names(result_tidy$token_scores), collapse = ", "), "\n\n")

# Key differences:
# - Custom tokenizer: Preserves URLs and phone numbers as special tokens
# - Tidytext tokenizer: Removes punctuation, may split URLs differently
# - For spam detection, special tokens (URLs, numbers) are valuable signals!

# -----------------------------------------------------------------------------

## Summary and Key Takeaways ----

# 1. Log odds extend naturally from logistic regression to text classification
#    - Each word contributes its own log odds
#    - We sum across words to get overall classification
#    - Based on Naive Bayes assumption (words are independent)
#
# 2. The process:
#    - Build contingency table (word counts in each class)
#    - Calculate log odds: log((a*d)/(b*c))
#    - Sum log odds across all words in document
#    - Classify based on whether sum is positive (ham) or negative (spam)
#      (Because we specified "ham" as target corpus in bind_logOdds)
#
# 3. Performance evaluation:
#    - Always compare to baseline
#    - Consider multiple metrics (accuracy, precision, recall, F1)
#    - Think about the cost of different error types
#
# 4. Preprocessing matters:
#    - Tokenization affects which features the model sees
#    - Domain-specific processing (URLs, numbers) can improve performance
#    - Custom tokenizers let you preserve important signals

# EXTENSION: Can you improve the classifier?
# Ideas to try:
# - Add bigrams (two-word sequences) as features
# - Filter out very rare words
# - Use TF-IDF weighting instead of raw counts
# - Try different threshold values (currently using 0)
# - Experiment with different tokenizers
