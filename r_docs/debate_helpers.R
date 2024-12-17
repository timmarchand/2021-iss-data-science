## Helper functions for the all_debates data set

## classify debaters

classify_debaters <- function(data, target_speakers = c("KENNEDY", "NIXON"), year_range = NULL, sample_prop = 0.9, top_n_features = 20) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rsample)
  library(tidytext)
  library(caret)
  library(patchwork)

  # Filter by year if year_range is provided
  if (!is.null(year_range)) {
    if (length(year_range) == 1) {
      data <- data %>% filter(lubridate::year(date) == year_range)
    } else if (length(year_range) == 2) {
      data <- data %>% filter(lubridate::year(date) >= year_range[1] & lubridate::year(date) <= year_range[2])
    } else {
      stop("year_range must be either a single year or a range of two years.")
    }
  }

  # Add color for later plotting
  data <- data %>% 
    mutate(fill1 = case_when(party == "REP" ~ "#d30b0d",
                             party == "DEM" ~ "#0033cc",
                             TRUE ~ "#cbcaca"),
           fill2 = case_when(party == "REP" ~ "#d27979",
                             party == "DEM" ~ "#428bca",
                             TRUE ~ "#cbcaca"))

  # Filter dataset for target speakers
  data <- data %>% filter(speaker %in% target_speakers)

  speaker_fill <- data %>% 
    mutate(fill = fill1) %>% 
    distinct(speaker, fill, fill2)

  # Check if they are the same party
  same_party <- data %>% distinct(party) %>% nrow() == 1
  if (same_party) {
    speaker_fill[2, 2] <- speaker_fill[2, 3]
  }

  # Select only relevant columns
  filtered_data <- data %>% select(turn, speaker, text)

  # Stratified sampling
  split <- rsample::initial_split(filtered_data, strata = speaker, prop = sample_prop)
  train <- rsample::training(split)
  test <- rsample::testing(split)

  # Compute log-odds scores for tokens
  train_scores <- train %>%
    unnest_tokens("token", text) %>%
    count(speaker, token) %>%
    pivot_wider(names_from = speaker, values_from = n, values_fill = 0) %>%
    bind_logOdds(target_speakers[1], target_speakers[2]) %>%
    select(token, score = log_odds)

  # Apply log-odds scores to test data
  test_fit <- test %>%
    rowid_to_column(var = "case") %>% 
    unnest_tokens("token", text) %>%
    left_join(train_scores, by = "token") %>%
    mutate(score = replace_na(score, 0)) %>%
    group_by(case, speaker) %>%
    summarize(turn_score = sum(score), .groups = "drop") %>%
    mutate(prediction = ifelse(turn_score > 0, target_speakers[1], target_speakers[2]))

  # Evaluate predictions using caret::confusionMatrix
  cm <- caret::confusionMatrix(
    factor(test$speaker, levels = target_speakers),  # Truth
    factor(test_fit$prediction, levels = target_speakers)  # Predictions
  )

  # Prepare top features for plotting
  model_scores <- train_scores %>%
    mutate(prediction = ifelse(score > 0, target_speakers[1], target_speakers[2]))

  p1 <- model_scores %>% 
    filter(prediction == target_speakers[1]) %>%
    slice_max(score, n = top_n_features) %>%
    ggplot(aes(x = reorder(token, score), y = score)) +
    geom_col(fill = speaker_fill$fill[1]) +
    coord_flip() +
    labs(title = paste("Top", top_n_features, "Features for", target_speakers[1]),
         x = "Word",
         y = "Log-Odds")

  p2 <- model_scores %>% 
    filter(prediction == target_speakers[2]) %>%
    slice_min(score, n = top_n_features) %>%
    ggplot(aes(x = reorder(token, score), y = abs(score))) +
    geom_col(fill = speaker_fill$fill[2]) +
    coord_flip() +
    labs(title = paste("Top", top_n_features, "Features for", target_speakers[2]),
         x = "Word",
         y = "Log-Odds")
  
  ## Compare top features by log-odds
top_features_plot <- model_scores %>%
  inner_join(speaker_fill, join_by("prediction" == "speaker")) %>%
  slice_max(abs(score), n = top_n_features) %>%
  ggplot(aes(x = reorder(token, score), y = score, fill = speaker)) +
  geom_col(aes(fill = fill)) +
  scale_fill_identity(
    name = "Speaker",  # Legend title
    labels = c(target_speakers[1], target_speakers[2])) +
  coord_flip() +
  labs(title = "Top Features by Log-Odds",
       x = "Word",
       y = glue::glue("Log-Odds ({target_speakers[1]} vs. {target_speakers[2]})")) +
  theme_minimal() +
    theme(legend.position = "right")

test$prediction <- test_fit$prediction

  # Accuracy vs. word count
  accuracy_plot <- test %>% 
    mutate(word_count = str_count(text, "\\w+")) %>%
    distinct(turn, speaker, prediction, text, word_count) %>%
    mutate(accurate = speaker == prediction) %>%
    ggplot(aes(word_count, accurate, color = accurate)) +
    geom_jitter(alpha = 0.4, height = 0.1) +
    labs(title = "Prediction Accuracy by Word Count",
         x = "Word Count",
         y = "Accurate") +
    theme_minimal() +
    theme(legend.position = "none")

  # Return results
  list(
    text_data = filtered_data,
    confusion_matrix = cm,
    model_scores = model_scores,
    top_features_plots = p1 + p2,
    top_features_comparison = top_features_plot,
    accuracy_plot = accuracy_plot
  )
}

## generate_kwic

generate_kwic <- function(data, 
                          category_column = "speaker", 
                          text_column = "text", 
                          speakers = NULL, 
                          keyword = "\\bAmerica", 
                          year_range = NULL,
                          context_words = 10,
                          separated = FALSE,
                          tokenize = TRUE) {
  
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(dtagger)
  
      # Optionally filter by year
   if (!is.null(year_range)) {
    if (length(year_range) == 1) {
      # Single year filtering
      data <- data %>%
        filter(year(date) == year_range)
    } else if (length(year_range) == 2) {
      # Range filtering
      data <- data %>%
        filter(year(date) >= year_range[1] & year(date) <= year_range[2])
    } else {
      stop("year_range must be either a single year or a range of two years.")
    }
   }
  
    # Optionally filter by speakers
  if (!is.null(speakers)) {
    data <- data %>%
      filter(!!sym(category_column) %in% speakers)
  }
  
  
  # Process data to extract KWIC
  kwic_data <- data %>%
    mutate(kwic = map(!!sym(text_column), ~quick_conc(.x, keyword, tokenize = TRUE, n = context_words, separated = separated))) %>%
    unnest(kwic) %>%
    relocate(!!sym(category_column), .after = token_id ) %>% 
    select(any_of(c("debaters", "date", "type")), !!sym(category_column):last_col() )
  
  return(kwic_data)
}

tidy_cm <- function(cm){
  broom::tidy(cm) %>% 
    filter(term %in% c("accuracy", "recall", "precision", "f1"))
}
