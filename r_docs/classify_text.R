classify_text <- function(text, 
                          model, 
                          show_scores = FALSE, 
                          category = "speaker",
                          tokenizer = c("tidy", "clean", "ptb")) {
  library(tidyverse)
  library(tidytext)
  library(tokenizers)
  
  # Validate and set tokenizer
  tokenizer <- match.arg(tokenizer)
  
  # Extract the two categories (e.g., speakers) from the model dataframe
  categories <- model %>%
    distinct(!!sym(category)) %>%
    pull(!!sym(category))
  
  if (length(categories) != 2) {
    stop("The model dataframe must include exactly two categories.")
  }
  
  # Tokenize the input text
  tokens <- switch(
    tokenizer,
    tidy = tibble(text) %>% unnest_tokens("token", text),
    ptb = tibble(text) %>% 
      transmute(token = tokenize_ptb(text)) %>%
      unnest(cols = token),
    clean = {
      string_cleaner <- function(msg) {
        msg %>%
          str_remove_all("[^[:alnum:] ]+") %>%
          str_to_lower() %>%
          str_replace_all("\\b(http|www).+?\\b", "_url_") %>%
          str_replace_all("\\b((\\d|-){7,})\\b", "_longnum_") %>%
          str_split_1(" ") %>%
          keep(~ nchar(.x) > 0)
      }
      tibble(text) %>%
        transmute(token = map(text, string_cleaner)) %>%
        unnest(cols = token)
    }
  )
  
   # Match tokens with model_scores and sum the log-odds
  token_scores <- tokens %>%
    left_join(model, by = "token") %>%  # Keep original token order
    mutate(log_odds = replace_na(log_odds, 0)) %>%  # Fill missing log_odds with 0
    select(token, log_odds) %>%
    deframe() 
  
  # Calculate the total score
  total_score <- sum(token_scores, na.rm = TRUE)
  
  # Classify based on the total score
  classification <- ifelse(total_score > 0, categories[1], categories[2])
  
  # Return results
  if (show_scores) {
    return(list(
      classification = classification,
      total_score = total_score,
      token_scores = token_scores
    ))
  }
  classification
}
