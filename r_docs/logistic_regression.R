# Logistic regression in R: example
library(tidyverse)
library(broom) # for clean regression tables
library(rsample) # for model training and testing
library(palmerpenguins) # for penguins!

## GPA example ---- 

library(tidyverse)
theme_set(theme_minimal())
admissions <- read_csv("https://raw.githubusercontent.com/equitable-equations/youtube/refs/heads/main/Logistic%20regression%20in%20R/admissions.csv")
write_csv(admissions, "data/admissions.csv")


glimpse(admissions)

## what does this plot show?
## what's the relationship between gpa and college admission?
ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1)

## what happens when we add a linear trend line?
ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1) +
  geom_smooth(method = "lm")


## create a "generalized" linear model from the "binomial" family
model <- glm(admitted ~ gpa, 
             data = admissions,
             family = "binomial")
summary(model)

broom::tidy(model)

## This is what the coefficients mean:

## logit(p) = -12 + 4.08.gpa
## log(p / 1-p) = 12 + 4.08.gpa
## p = e^(-12 + 4.08gpa) / (1 + e^(-12 + 4.08gpa))

ggplot(admissions, aes(x = gpa, 
                       y = admitted)) +
  geom_jitter(height = .05, 
              alpha = .1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

adm_sum <- admissions %>% 
  group_by(gpa) %>% 
  summarize(prop_adm = mean(admitted),
            count = n())
adm_sum

ggplot(adm_sum, aes(x = gpa, 
                    y = prop_adm)) +
  geom_point()

model2 <- glm(prop_adm ~ gpa, 
              family = "binomial",
              data = adm_sum, 
              weights = count)
summary(model2)


ggplot(adm_sum, aes(x = gpa, 
                    y = prop_adm,
                    weight = count)) +
  geom_point() +
  geom_smooth(method = "glm",
              se = FALSE,
              method.args = list(family = "binomial"))


# Example 2 -- classifying penguins by variable(s) ----

## Task - try to build a classifier to identify a Gentoo penguin,
## based on  variables from plamerpenguins

## Create a "dummy variable" for gentoo
## gentoo = 1 when TRUE

penguins <- penguins %>% 
              mutate(gentoo = as.integer(species == "Gentoo"))


# if we randomly pick up a penguin, what is the probability it will be a Gentoo?

penguins %>% 
  count(species) %>% 
  mutate(prop = n/sum(n))


# most simple model - say every  penguin we pick up is NOT a Gentoo
# 64% chance of being correct
# Can we create a classifier that does better?

## When building a classifier, important to split our data into 
## training and testing parts
## Only use the testing split after we have chosen our classification model

# Split data

set.seed(42)
split <- initial_split(penguins, 
                       prop = .80, 
                       strata = gentoo) 
pen_train <- training(split)
pen_test <- testing(split)


# Visualize the data

ggplot(pen_train, aes(x = body_mass_g,
                       y = gentoo)) +
  geom_jitter(height = .05, 
              alpha = .5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
  # facet_wrap(~island) +
  theme_minimal()

# Build a simple model

model1 <- glm(gentoo ~ body_mass_g,
             data = pen_train,
             family = "binomial")
summary(model1)

model1 %>% 
  broom::tidy()

# evaluate the model on the testing set

pen_test <- pen_test %>% 
  mutate(gentoo_prob = predict(model1,
                               pen_test,
                               type = "response"),
         gentoo_pred = ifelse(gentoo_prob > .5, 1, 0))



## confusion matrix

cm<- table(
  Truth = pen_test$gentoo,
  Prediction = pen_test$gentoo_pred
)
cm

## Table shows:
## True Negatives (TN)  | False Negatives (FN)
## False Positives (FP) | True Positives (TP)

## How to measure accuracy?
##  TP + TN / TOTAL
sum(diag(cm)) / sum(cm) # XX% accuracy

## More details from caret package alternative
cm <- confusionMatrix(
  factor(pen_test$gentoo, levels = c(0, 1)),  # Truth
  factor(pen_test$gentoo_pred, levels = c(0, 1))  # Prediction
)
cm

tidy(cm)


#' Evaluate the performance of a binary classifier.
#' Compare models using Accuracy, Precision, Recall, and F1.
#' Interpret Kappa for understanding classification agreement beyond chance.
#' 
#' Some indicators:
#' 
#' Accuracy = TP + TN / TOTAL
#' 
#' Kappa = Measures agreement between predicted and truth, accounting for chance
#' 
#' Sensitivity (Recall) = The proportion of actual 1s correctly identified
#'                      = TP / (TP + FN)
#'                      
#' Precision = The proportion of predicted 1s that are actually 1
#'           = TP / (TP + FP)
#'           
#' F1 = The harmonic mean of Precision and Recall (Sensitivity)
#'    = 2(Precision.Recall) / (Precision + Recall)


## Example 3 -- classifying penguins by multiple variables ----
## Add island as variable

# Split data

split <- initial_split(penguins, 
                       prop = .80, 
                       strata = gentoo) 
pen_train <- training(split)
pen_test <- testing(split)


# Visualize the data

ggplot(pen_train, aes(x = body_mass_g,
                       y = gentoo)) +
  geom_jitter(height = .05, 
              alpha = .5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
   facet_wrap(~island) +
  theme_minimal()

# Build a  model with two variables

model2 <- glm(gentoo ~ body_mass_g + island,
             data = pen_train,
             family = "binomial")
summary(model2)

model2 %>% 
  broom::tidy()

# evaluate the model on the testing set

pen_test <- pen_test %>% 
  mutate(gentoo_prob = predict(model2,
                               pen_test,
                               type = "response"),
         gentoo_pred = ifelse(gentoo_prob > .5, 1, 0))



## confusion matrix

cm<- table(
  Truth = pen_test$gentoo,
  Prediction = pen_test$gentoo_pred
)
cm

## Table shows:
## True Negatives (TN)  | False Negatives (FN)
## False Positives (FP) | True Positives (TP)

## How to measure accuracy?
##  TP + TN / TOTAL
sum(diag(cm)) / sum(cm) # XX% accuracy

## More details from caret package alternative
cm <- confusionMatrix(
  factor(pen_test$gentoo, levels = c(0, 1)),  # Truth
  factor(pen_test$gentoo_pred, levels = c(0, 1))  # Prediction
)
cm

tidy(cm)

## Which model is better??

## TASK - can you find a better fitting model? ----


## Relationship between log odds and probability ---- 

library(ggplot2)

# Generate data 
p <- seq(0.01, 0.99, length.out = 100)  # Avoid 0 and 1 to prevent division by zero
logit <- log(p / (1 - p))
data <- tibble(Probability = p, Logit = logit)

# Create the plot
ggplot(data, aes(x = Probability, y = Logit)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey", size = 0.8) +
  labs(
    title = "Logit (Log Odds) as a Function of Probability",
    x = "Probability (p)",
    y = "Logit (Log Odds)"
  ) 
