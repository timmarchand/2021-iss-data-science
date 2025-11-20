#' Classify Text Using Log Odds Scores
#'
#' Classifies a text string into one of two categories by summing the log odds 
#' scores of its tokens and adding a bias term. This function implements a 
#' simple Naive Bayes-style text classifier based on word-level log odds ratios.
#'
#' @param text A character string containing the text to classify.
#' @param model A data frame containing token scores with at least two columns:
#'   \itemize{
#'     \item \code{token}: The word/token
#'     \item \code{log_odds}: The log odds ratio for that token
#'     \item A category column (specified by \code{category} parameter) with exactly two unique values
#'   }
#' @param bias Numeric. A class prior or intercept term to add to the total score. 
#'   Defaults to 0 (neutral). Use this to account for unbalanced classes (e.g., 
#'   if Spam is rare, providing a negative bias makes the classifier conservative).
#' @param show_scores Logical. If \code{TRUE}, returns a list with detailed scores 
#'   for each token. If \code{FALSE} (default), returns only the classification.
#' @param category Character string specifying the name of the column in \code{model} 
#'   that contains the category labels (e.g., "speaker", "label", "author"). Default is "speaker".
#' @param tokenizer Character string specifying which tokenization method to use:
#'   \itemize{
#'     \item \code{"tidy"} (default): Uses \code{tidytext::unnest_tokens()}, removes punctuation
#'     \item \code{"clean"}: Custom tokenizer that preserves URLs as "_url_" and long numbers as "_longnum_"
#'     \item \code{"ptb"}: Penn Treebank tokenizer via \code{tokenizers::tokenize_ptb()}
#'   }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Tokenizing the input text according to the specified method
#'   \item Looking up the log odds score for each token in the model
#'   \item Assigning a score of 0 to tokens not found in the model
#'   \item Summing all token scores and adding the \code{bias} term
#'   \item Classifying based on the sign of the total score:
#'     \itemize{
#'       \item Positive total score (> 0) → First category found in \code{model}
#'       \item Negative total score (≤ 0) → Second category found in \code{model}
#'     }
#' }
#'
#' @return 
#' If \code{show_scores = FALSE}: A character string with the predicted category.
#' 
#' If \code{show_scores = TRUE}: A list with three elements:
#' \itemize{
#'   \item \code{classification}: The predicted category (character string)
#'   \item \code{total_score}: The sum of token log odds scores + bias (numeric)
#'   \item \code{token_scores}: A named numeric vector of log odds scores for each token
#' }
#'
#' @export
classify_text <- function(text, 
                          model, 
                          bias = 0,
                          show_scores = FALSE, 
                          category = "speaker",
                          tokenizer = c("tidy", "clean", "ptb")) {
  library(tidyverse)
  library(tidytext)
  library(tokenizers)
  
  # Validate and set tokenizer
  tokenizer <- match.arg(tokenizer)
  
  # Extract the two categories
  # NOTE: distinct() keeps order of appearance. 
  # We assume the model is structured such that the Target Class appears first
  # or that the log_odds are calculated relative to the first category.
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
    left_join(model, by = "token") %>% 
    mutate(log_odds = replace_na(log_odds, 0)) %>% 
    select(token, log_odds) %>%
    deframe() 
  
  # Calculate the total score
  total_score <- sum(token_scores, na.rm = TRUE) + bias
  
  # Classify based on the total score
  # Positive Score = First Category found in the model
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
