# Install required packages if not already installed
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
# Load libraries
library(tidyverse)
library(nnet) ## for multinomial regression
library(plotly) ## for interactive plots
library(palmerpenguins) ## for data
library(rsample) ## for model split
library(broom) ## for tidy results
library(caret) ## for confusion matrix

# set theme for plots
theme_set(theme_minimal())
# Load the dataset
data("penguins")


# Inspect the dataset
glimpse(penguins)

# Remove rows with missing values
penguins <- na.omit(penguins)

# Split the data into training and testing sets (e.g., 80-20 split)
set.seed(123)  # Set seed for reproducibility
split <- initial_split(penguins, prop = 0.8)

# Create training and testing sets
train_data <- training(split)
test_data <- testing(split)

# Fit the models using training data
model1 <- multinom(species ~ bill_length_mm, data = train_data)
model2 <- multinom(species ~ bill_length_mm + island, data = train_data)
model3 <- multinom(species ~ bill_length_mm + body_mass_g + island, data = train_data)

# Summarize the model
summary(model1)

# Tidy summary of model
tidy(model1)
tidy(model2)
tidy(model3)


# Predict species on the testing set
test_predictions1 <- predict(model1, newdata = test_data)
test_predictions2 <- predict(model2, newdata = test_data)
test_predictions3 <- predict(model3, newdata = test_data)

# Create a quick confusion matrix
conf_matrix1 <- table(Predicted = test_predictions1, Actual = test_data$species)
conf_matrix2 <- table(Predicted = test_predictions2, Actual = test_data$species)
conf_matrix3 <- table(Predicted = test_predictions3, Actual = test_data$species)

# Check
conf_matrix1
conf_matrix2
conf_matrix3

# # Ensure both predictions and actual values are factors with the same levels
# test_predictions1 <- factor(test_predictions1, levels = levels(test_data$species))
# test_predictions2 <- factor(test_predictions2, levels = levels(test_data$species))
# test_predictions3 <- factor(test_predictions3, levels = levels(test_data$species))


# Generate confusion matrix with caret
actual_species <- factor(test_data$species, levels = levels(test_data$species))

cm1 <- confusionMatrix(test_predictions1, actual_species) %>% 
  tidy() %>% 
  filter(term %in% c("accuracy", "precision", "recall", "f1"))

cm2 <- confusionMatrix(test_predictions2, actual_species) %>% 
  tidy() %>% 
  filter(term %in% c("accuracy", "precision", "recall", "f1"))

cm3 <- confusionMatrix(test_predictions3, actual_species) %>% 
  tidy() %>% 
  filter(term %in% c("accuracy", "precision", "recall", "f1"))

# Check
cm1
cm2
cm3

# Get predicted probabilities for the testing set
probabilities1 <- predict(model1, newdata = test_data, type = "probs")
probabilities2 <- predict(model2, newdata = test_data, type = "probs")
probabilities3 <- predict(model3, newdata = test_data, type = "probs")

probabilities_df1 <- test_data %>% 
  bind_cols(probabilities1)

probabilities_df2 <- test_data %>% 
  bind_cols(probabilities2)

probabilities_df3 <- test_data %>% 
  bind_cols(probabilities3)

p1 <- probabilities_df1 %>% 
  pivot_longer(cols = Adelie:Gentoo, 
               names_to = "Predicted Species", 
               values_to = "probability", 
               values_transform = ~round(.x,2)) %>% 
  ggplot() +
  aes(x = bill_length_mm, y = probability, colour = `Predicted Species` ) + 
  geom_point(size = 0.5) + 
  facet_grid(scales = "free")


p1
## Use plotly
ggplotly(p1)

p2 <- probabilities_df2 %>% 
  pivot_longer(cols = Adelie:Gentoo, 
               names_to = "Species", 
               values_to = "probability", 
               values_transform = ~round(.x,2)) %>% 
  ggplot() +
  aes(x = bill_length_mm, y = probability, colour = Species) + 
  geom_point(size = 0.5) + 
  facet_wrap(~island, nrow = 3)

p2
## Use plotly
ggplotly(p2)

probabilities_df3 %>% 
  pivot_longer(cols = Adelie:Gentoo, names_to = "Predicted Species", values_to = "probability") %>% 
  ggplot() +
  aes(x = bill_length_mm, y = probability, colour = `Predicted Species`) + 
  geom_line() +
  facet_wrap(~island, nrow = 3)

plotly::ggplotly(p1)

probabilities_df3 %>% 
  rowid_to_column(var = "case") %>% 
  pivot_longer(cols = Adelie:Gentoo, 
               names_to = "Predicted_Species", 
               values_to = "probability", 
               values_transform = ~round(.x,2)
               ) %>%
  group_by(case) %>%
filter(probability == max(probability)) %>%
  ungroup() %>%
  plotly::plot_ly(
        x = ~body_mass_g,
        y = ~bill_length_mm,
        z = ~probability,
        color = ~Predicted_Species,
        size = 0.5,
        type = "scatter3d")
