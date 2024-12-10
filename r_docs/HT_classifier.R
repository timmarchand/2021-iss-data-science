HT <- read_csv("data/HT_dbt.csv") %>% 
  rowid_to_column(var = "turn") %>% 
  filter(speaker %in% c("TRUMP", "HARRIS")) 


# HT2 <- HT %>% 
#   mutate(turn = as.numeric(speaker != lag(speaker)),
#          turn = replace_na(turn, 0) %>% cumsum) %>% 
#   summarise(text = str_c(text, collapse = " "), .by = c(speaker, turn))

# make sure to do stratified sampling
split <- rsample::initial_split(HT, strata = speaker, prop = 0.9)
train <- rsample::training(split)
test <- rsample::testing(split)

train_scores <- train %>% 
  unnest_tokens("token", text) %>% 
  count(speaker, token) %>% 
  pivot_wider(names_from = speaker, values_from = n, values_fill = 0) %>% 
  bind_logOdds("HARRIS","TRUMP") %>% 
  remove_contingency_table() %>% 
  select(token, score = log_odds) 

test_fit <- test %>% 
  filter(speaker %in% c("TRUMP", "HARRIS")) %>% 
  unnest_tokens("token", text) %>% 
  left_join(train_scores) %>% 
  mutate(score = replace_na(score, 0)) %>% 
  mutate(turn_score = sum(score), .by = turn) %>% 
  mutate(prediction = ifelse(turn_score > 0, "HARRIS", "TRUMP"))


## More details from caret package alternative
cm <- confusionMatrix(
  factor(test_fit$speaker, levels = c("HARRIS", "TRUMP")),  # Truth
  factor(test_fit$prediction, levels = c("HARRIS", "TRUMP"))  # PrediHTion
)
tidy(cm) %>% 
  filter(term %in% c("accuracy", "recall", "precision","f1"))

test_fit %>% 
  inner_join(HT) %>% 
  mutate(word_count = str_count(text, "\\w+")) %>% 
  distinct(turn, speaker, prediction, text, word_count) %>% 
  mutate(accurate = speaker == prediction)  %>% 
  ggplot() +
  aes(word_count, accurate) +
  geom_jitter(alpha = 0.4, height = 0.1)
  
  
## Plot the results

train_scores %>% 
  rename(log_odds = score) %>% 
    mutate(speaker = ifelse(log_odds > 0, "HARRIS","TRUMP")) %>% 
  slice_max(abs(log_odds), n = 50) %>% 
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = speaker)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top Features by Log-Odds",
       x = "Word",
       y = "Log-Odds (HARRIS vs. TRUMP)") +
  theme_minimal()

## Pick out key words
HT %>% 
  mutate(kwic = map(text, ~quick_conc(.x, "\\b[Ww]ho\\b", tokenize = TRUE))) %>% 
  unnest(kwic) %>% 
    select(speaker, left:right) %>% 
  View


