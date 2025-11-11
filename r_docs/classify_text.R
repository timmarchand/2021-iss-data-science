#' Classify Text Using Log Odds Scores
#'
#' Classifies a text string into one of two categories by summing the log odds 
#' scores of its tokens. This function implements a simple Naive Bayes-style 
#' text classifier based on word-level log odds ratios.
#'
#' @param text A character string containing the text to classify.
#' @param model A data frame containing token scores with at least two columns:
#'   \itemize{
#'     \item \code{token}: The word/token
#'     \item \code{log_odds}: The log odds ratio for that token
#'     \item A category column (specified by \code{category} parameter) with exactly two unique values
#'   }
#' @param show_scores Logical. If \code{TRUE}, returns a list with detailed scores 
#'   for each token. If \code{FALSE} (default), returns only the classification.
#' @param category Character string specifying the name of the column in \code{model} 
#'   that contains the category labels (e.g., "speaker", "label"). Default is "speaker".
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
#'   \item Summing all token scores to get a total score
#'   \item Classifying based on the sign of the total score:
#'     \itemize{
#'       \item Positive total score → first category (alphabetically)
#'       \item Negative total score → second category (alphabetically)
#'     }
#' }
#'
#' The \code{model} data frame should be created using \code{bind_logOdds()} and 
#' contain log odds ratios that indicate how strongly each token is associated 
#' with one category versus another.
#'
#' @return 
#' If \code{show_scores = FALSE}: A character string with the predicted category.
#' 
#' If \code{show_scores = TRUE}: A list with three elements:
#' \itemize{
#'   \item \code{classification}: The predicted category (character string)
#'   \item \code{total_score}: The sum of all token log odds scores (numeric)
#'   \item \code{token_scores}: A named numeric vector of log odds scores for each token
#' }
#'
#' @examples
#' \dontrun{
#' # Assume you have a trained model from spam detection
#' model <- train_data %>%
#'   transmute(label, token = map(text, string_cleaner)) %>%
#'   unnest(cols = token) %>%
#'   count(label, token) %>%
#'   pivot_wider(names_from = label, values_from = n, values_fill = 0) %>%
#'   bind_logOdds("ham", "spam") %>%
#'   select(token, log_odds, label)
#'
#' # Simple classification
#' classify_text("Click here for FREE prizes!", 
#'               model = model, 
#'               category = "label",
#'               tokenizer = "clean")
#' # Returns: "spam"
#'
#' # Detailed classification with token scores
#' result <- classify_text("Click here for FREE prizes!", 
#'                         model = model, 
#'                         show_scores = TRUE,
#'                         category = "label",
#'                         tokenizer = "clean")
#' 
#' result$classification  # "spam"
#' result$total_score     # e.g., 8.34
#' result$token_scores    # Named vector: click = 0.5, here = 1.2, free = 6.1, ...
#' }
#'
#' @seealso 
#' \code{\link{bind_logOdds}} for creating the model scores,
#' \code{\link{bind_contingency_table}} for computing token frequencies
#'
#' @export
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
