# Logistic Regression in R
library(tidyverse)
library(broom) # for clean regression tables
library(rsample) # for model training and testing
library(palmerpenguins) # for penguins!

## Learning Objectives ----

# By the end of this session, you will be able to:
# 1. Explain when and why to use logistic regression instead of linear regression
# 2. Interpret logistic regression coefficients in terms of log odds and probability
# 3. Build and evaluate binary classification models using train/test splits
# 4. Compare competing models using accuracy, AIC, and confusion matrix metrics

# -----------------------------------------------------------------------------

## Why Logistic Regression? ----

# Linear regression predicts CONTINUOUS outcomes (e.g., body mass, temperature, income)
# But what if we want to predict BINARY outcomes? (e.g., yes/no, pass/fail, spam/ham)
#
# Problem with linear regression for binary outcomes:
# - Can predict values < 0 or > 1 (impossible for probabilities!)
# - Assumes constant variance (but binary data is more variable near p=0.5)
# - Violates normality assumption
#
# Solution: LOGISTIC REGRESSION
# - Predicts probability (always between 0 and 1)
# - Uses a logit link function: log(p/(1-p)) = β₀ + β₁X
# - Models the LOG ODDS of the outcome

# -----------------------------------------------------------------------------

## Example 1: GPA and College Admissions ----

library(tidyverse)
theme_set(theme_minimal()) # for clean plots
admissions <- read_csv("https://raw.githubusercontent.com/equitable-equations/youtube/refs/heads/main/Logistic%20regression%20in%20R/admissions.csv")
write_csv(admissions, "data/admissions.csv")

glimpse(admissions)

# Our outcome variable is BINARY: admitted (0 = no, 1 = yes)
# Our predictor is CONTINUOUS: gpa

# Visualize: what's the relationship between GPA and admission?
ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1)

# What happens if we try to fit a LINEAR regression line?
ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1) +
  geom_smooth(method = "lm")

# PROBLEM: The line predicts values below 0 and above 1!
# These aren't valid probabilities.

# -----------------------------------------------------------------------------

## Fitting a Logistic Regression Model ----

# We use glm() with family = "binomial" for logistic regression
# glm = "Generalized Linear Model"
# binomial family = for binary (0/1) outcomes

model <- glm(admitted ~ gpa, 
             data = admissions,
             family = "binomial")

summary(model)

# Cleaner output with broom package:
broom::tidy(model)

## Understanding the coefficients:

# The model predicts LOG ODDS (not probability directly):
# log(p/(1-p)) = -12 + 4.08 × gpa
#
# Where p/(1-p) is the "odds" of admission
# Taking the log makes the relationship linear
#
# To get probability from log odds:
# p = e^(-12 + 4.08×gpa) / (1 + e^(-12 + 4.08×gpa))

# Interpretation:
# - Intercept (≈ -12): Log odds of admission when GPA = 0 (not meaningful here)
# - Slope (≈ 4.08): For each 1-point increase in GPA, log odds increase by 4.08
#   In other words: each GPA point multiplies the odds by e^4.08 ≈ 59 times!

# Visualize with the proper logistic curve:
ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

# Much better! The curve stays between 0 and 1.

# -----------------------------------------------------------------------------

## Example 2: Classifying Penguins ----

# Task: Build a classifier to identify Gentoo penguins
# based on physical measurements

# Create a binary outcome variable
# gentoo = 1 when species is Gentoo, 0 otherwise
penguins <- penguins %>% 
              mutate(gentoo = as.integer(species == "Gentoo"))

# What's our baseline accuracy?
# If we randomly pick a penguin, what's the probability it's a Gentoo?
penguins %>% 
  count(species) %>% 
  mutate(prop = n/sum(n))

# About 36% are Gentoo
# Simplest "model": Predict every penguin is NOT a Gentoo
# This would be correct 64% of the time (our BASELINE)
#
# Can we build a model that does better than 64%?

# -----------------------------------------------------------------------------

## Train/Test Split ----

# CRITICAL: When building predictive models, we must:
# 1. Train the model on one subset of data (TRAINING set)
# 2. Evaluate it on a different subset (TESTING set)
#
# Why? To see if the model generalizes to new data!
# If we test on the same data we trained on, we're cheating.

set.seed(42)  # for reproducibility
split <- initial_split(penguins, 
                       prop = .80,         # 80% train, 20% test
                       strata = gentoo)    # maintain same gentoo proportion in both sets
pen_train <- training(split)
pen_test <- testing(split)

# -----------------------------------------------------------------------------

## Model 1: Body Mass Only ----

# Visualize the relationship first:
ggplot(pen_train, aes(x = body_mass_g,
                       y = gentoo)) +
  geom_jitter(height = .05, 
              alpha = .5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
  theme_minimal()

# Gentoo penguins tend to be heavier!

# Fit the model:
model1 <- glm(gentoo ~ body_mass_g,
             data = pen_train,
             family = "binomial")

summary(model1)

model1 %>% 
  broom::tidy()

# Interpretation:
# Positive coefficient for body_mass_g means heavier penguins are more likely to be Gentoo
# Each additional gram increases the log odds of being Gentoo

# -----------------------------------------------------------------------------

## Evaluating Model 1 ----

# Use our model to predict on the TEST set
pen_test <- pen_test %>% 
  mutate(gentoo_prob = predict(model1,           # Get predicted probabilities
                               pen_test,
                               type = "response"),
         gentoo_pred = ifelse(gentoo_prob > .5,  # Convert to binary prediction
                              1, 0))              # If prob > 0.5, predict Gentoo

# Confusion Matrix: Compare predictions to truth
cm <- table(
  Truth = pen_test$gentoo,
  Prediction = pen_test$gentoo_pred
)
cm

## The confusion matrix shows:
##                    Prediction
##           |   0 (Not Gentoo)  |  1 (Gentoo)  
## ----------|-------------------|-------------
## Truth  0  | True Negatives    | False Positives
##        1  | False Negatives   | True Positives

# Calculate accuracy: (True Positives + True Negatives) / Total
accuracy_model1 <- sum(diag(cm)) / sum(cm)
accuracy_model1  # Compare this to our 64% baseline!

# More detailed metrics using caret package:
library(caret)
cm1 <- confusionMatrix(
  factor(pen_test$gentoo, levels = c(0, 1)),      # Truth
  factor(pen_test$gentoo_pred, levels = c(0, 1))  # Prediction
)
cm1

tidy(cm1)

#' Key Performance Metrics:
#' 
#' Accuracy = (TP + TN) / Total
#'   Overall proportion of correct predictions
#' 
#' Sensitivity (Recall) = TP / (TP + FN)
#'   Of all actual Gentoos, what proportion did we identify?
#'   (Important when we don't want to MISS positive cases)
#' 
#' Specificity = TN / (TN + FP)
#'   Of all non-Gentoos, what proportion did we correctly identify?
#' 
#' Precision = TP / (TP + FP)
#'   Of all our positive predictions, what proportion were actually correct?
#'   (Important when we don't want FALSE ALARMS)
#' 
#' F1 Score = 2 × (Precision × Recall) / (Precision + Recall)
#'   Harmonic mean of Precision and Recall (balances both concerns)
#' 
#' Kappa = Agreement beyond what we'd expect by chance
#'   Accounts for the possibility of correct predictions happening randomly

# -----------------------------------------------------------------------------

## Model 2: Body Mass + Island ----

# Does knowing which island the penguin is from improve our classification?

# Visualize by island:
ggplot(pen_train, aes(x = body_mass_g,
                       y = gentoo)) +
  geom_jitter(height = .05, 
              alpha = .5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
   facet_wrap(~island) +
  theme_minimal()

# Interesting! Biscoe island has many more Gentoos

# Fit model with two predictors:
model2 <- glm(gentoo ~ body_mass_g + island,
             data = pen_train,
             family = "binomial")

summary(model2)

model2 %>% 
  broom::tidy()

# Interpretation:
# - body_mass_g: Still positive (heavier = more likely Gentoo)
# - islandDream: Negative coefficient = Gentoos less common on Dream island
# - islandTorgersen: Negative coefficient = Gentoos less common on Torgersen island
# (Biscoe is the reference category - omitted from model)

# -----------------------------------------------------------------------------

## Evaluating Model 2 ----

# Predict on test set:
pen_test <- pen_test %>% 
  mutate(gentoo_prob2 = predict(model2,
                                pen_test,
                                type = "response"),
         gentoo_pred2 = ifelse(gentoo_prob2 > .5, 1, 0))

# Confusion matrix:
cm2 <- confusionMatrix(
  factor(pen_test$gentoo, levels = c(0, 1)),      # Truth
  factor(pen_test$gentoo_pred2, levels = c(0, 1))  # Prediction
)
cm2

tidy(cm2)

accuracy_model2 <- cm2$overall["Accuracy"]
accuracy_model2

# -----------------------------------------------------------------------------

## Comparing Models ----

# Which model is better? Let's compare them systematically:

# 1. Compare accuracy on test set:
cat("Baseline accuracy (always predict 'not Gentoo'):", 0.64, "\n")
cat("Model 1 accuracy (body mass only):", accuracy_model1, "\n")
cat("Model 2 accuracy (body mass + island):", accuracy_model2, "\n")

# 2. Compare AIC (Akaike Information Criterion):
#    Lower AIC = better model
#    AIC balances model fit with complexity (penalizes extra parameters)
cat("\nModel 1 AIC:", AIC(model1), "\n")
cat("Model 2 AIC:", AIC(model2), "\n")

# 3. Likelihood Ratio Test:
#    Statistical test: Does adding island significantly improve the model?
anova(model1, model2, test = "Chisq")

# If p-value < 0.05, the more complex model (model2) is significantly better

# Decision: Choose the model that:
# - Has better accuracy on test data
# - Has lower AIC
# - Has significantly better fit (likelihood ratio test)
# - Matches your priorities (e.g., if false positives are costly, prioritize precision)

# -----------------------------------------------------------------------------

## TASK: Can you build an even better model? ----

# Try adding other predictors:
# - bill_length_mm
# - bill_depth_mm  
# - flipper_length_mm
# - sex

# Hints:
# 1. Start by visualizing relationships with gentoo
# 2. Build several competing models
# 3. Evaluate each on the test set
# 4. Compare using accuracy, AIC, and the likelihood ratio test
# 5. Watch out for overfitting - more complex isn't always better!

# Can you beat model2's accuracy?

# -----------------------------------------------------------------------------

## Reference: Logit and Probability Relationship ----
## (For  reference during explanation)

library(ggplot2)

# Generate data 
p <- seq(0.01, 0.99, length.out = 100)
logit <- log(p / (1 - p))
data <- tibble(Probability = p, Logit = logit)

# The logit transformation converts probability (0 to 1) into log odds (-∞ to +∞)
# This allows us to use linear regression techniques
ggplot(data, aes(x = Probability, y = Logit)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey", size = 0.8) +
  labs(
    title = "Logit (Log Odds) as a Function of Probability",
    x = "Probability (p)",
    y = "Logit (Log Odds)"
  ) 
