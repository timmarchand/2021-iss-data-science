### Learn more about penguins!

## Learning Objectives

# By the end of this session, you will be able to:
# 1. Choose appropriate visualizations for different variable types (categorical/continuous, univariate/bivariate)
# 2. Build and interpret linear regression models with multiple predictors
# 3. Recognize Simpson's Paradox and understand the importance of controlling for confounding variables

# -----------------------------------------------------------------------------

## Setup

# Load the packages we need:
pacman::p_load(tidyverse, ggpubr, vcd, broom, moderndive, sjPlot, gapminder, rstatix)

# Load the penguins dataset - real data collected from Palmer Station, Antarctica
pacman::p_load(palmerpenguins)
library(palmerpenguins)

# -----------------------------------------------------------------------------

## Getting to know the data

# First, let's explore what variables we have
glimpse(penguins)

# TASK 1: Look at the output above. With a partner, classify each variable as:
# - Categorical (nominal or binary) OR Continuous (integer or ratio)
# Write your answers in the chat.

# Here's what we have:
# Categorical:
## nominal = species / island 
## binary = sex

# Continuous:
## integer = flipper_length_mm / body_mass_g / year
## ratio = bill_length_mm / bill_depth_mm

# Now get a statistical summary
summary(penguins)

# Notice: 
# - Three years of data (2007-2009)
# - Almost 50/50 split for sex
# - 11 missing values (NAs) in some variables
# - Fewer Chinstrap penguins and fewer penguins from Torgersen island

# -----------------------------------------------------------------------------

## Visualizing univariate data (one variable at a time)

# RULE: Categorical data → bar/column charts
#       Continuous data → histograms or density plots

### Categorical variables: counting things

penguins %>% count(species)
penguins %>% count(island)
penguins %>% count(sex)

# Can we visualize categorical data with a histogram? Let's try:

penguins %>% 
  ggplot(aes(species)) +
  geom_histogram()

# ERROR! Histograms are for continuous data. 
# We can force it with stat = "count", but better to use the right geom:

penguins %>% 
  ggplot(aes(species)) +
  geom_histogram(stat = "count")

# Better: use geom_bar for categorical data
penguins %>% 
  ggplot(aes(island)) +
  geom_bar()

# Or geom_col with count data (more flexibility for horizontal bars)
penguins %>% count(sex) %>% 
  ggplot(aes(n, sex)) +
  geom_col()

### Continuous variables: showing distributions

penguins %>% 
  ggplot(aes(bill_length_mm)) +
  geom_histogram()

# We can add color by species to see if distributions differ:
penguins %>% 
  ggplot(aes(bill_depth_mm, fill = species)) +
  geom_histogram() +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Or use facets to separate them completely:
penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_histogram() +
  facet_wrap(~species) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Density plots can be good for comparing groups:
penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# -----------------------------------------------------------------------------

## Visualizing bivariate data (two variables)

# RULE: Continuous vs Continuous → scatter plots
#       Categorical vs Continuous → boxplots or faceted histograms  
#       Categorical vs Categorical → mosaic plots

### Continuous vs Continuous: looking for relationships

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() 

# Add a regression line to see the trend:
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# QUESTION: What direction is this relationship? Keep this in mind - we'll return to it!

# Now let's add species information with color and shape:
penguins %>% 
  ggplot(aes(bill_length_mm, body_mass_g, color = species, shape = species)) +
  geom_point() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 

# Here's another relationship with separate regression lines per species:
# Note: flipper_length (explanatory) on x-axis, body_mass (response) on y-axis
penguins %>% 
  ggplot(aes(flipper_length_mm, body_mass_g, color = species, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

### Categorical vs Continuous: comparing groups

# Faceted histograms work well:
penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_histogram() +
  facet_wrap(~species) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Boxplots are more compact for comparisons:
# Categorical variable (species) on x-axis, continuous (body_mass_g) on y-axis
penguins %>% 
  ggplot(aes(species, body_mass_g, fill = species)) +
  geom_boxplot()  +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Jitter plots show individual points:
# Categorical variable (species) on x-axis, continuous (flipper_length) on y-axis
penguins %>% 
  ggplot(aes(species, flipper_length_mm)) +
  geom_jitter(aes(color = species, shape = species))  +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

# We can combine boxplot and jitter for maximum information:
# Categorical variable (island) on x-axis, continuous (body_mass_g) on y-axis
penguins %>% 
  ggplot(aes(island, body_mass_g)) +
  geom_boxplot() +
  geom_jitter(aes(color = species, shape = species))  +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

### Categorical vs Categorical: mosaic plots

# Mosaic plots show the relationship between categorical variables
# The area of each rectangle is proportional to the frequency

mosaic(~ species + island, data = penguins)

# Add shading to show significant associations:
mosaic(~ species + island, data = penguins, shade = TRUE)

mosaic(~ sex + island, data = penguins, shade = TRUE)

# We can even look at three-way relationships:
mosaic(~ species + island + sex, data = penguins, shade = TRUE)

# -----------------------------------------------------------------------------

## Statistical tests

### Correlations - for continuous vs continuous relationships

# Test the strength of association between body mass and flipper length:
penguins %>% 
  rstatix::cor_test(body_mass_g, flipper_length_mm)

# TASK 2: Run the correlation test below. Is this correlation positive or negative?
# What does this tell you about the relationship?
penguins %>% 
  rstatix::cor_test(bill_length_mm, bill_depth_mm)

### Chi-square test - for categorical vs categorical relationships

# Test if island and sex are independent:
penguins %>% 
  infer::chisq_test(island ~ sex)

# Test if island and species are independent:
penguins %>% 
  infer::chisq_test(island ~ species)

# -----------------------------------------------------------------------------

## Linear regression models

# Linear models let us predict one variable from others and test which predictors matter

### Models with categorical predictors

# Predict body mass from species:
cat_model <- lm(body_mass_g ~ species, data = penguins)

get_regression_summaries(cat_model)  # Overall model fit (R²)
get_regression_table(cat_model)      # Coefficients and p-values
get_regression_points(cat_model)     # Predictions and residuals

# Add sex as a second predictor:
cat_model2 <- lm(body_mass_g ~ species + sex, data = penguins)

get_regression_summaries(cat_model2)
get_regression_table(cat_model2)
get_regression_points(cat_model2)

# QUESTION: Did R² improve when we added sex to the model?

### Models with continuous predictors

# Simple model: predict body mass from flipper length
num_model <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
get_regression_summaries(num_model)
get_regression_table(num_model)
get_regression_points(num_model)

# Visualize the relationship:
# Model: body_mass_g ~ flipper_length_mm
# So flipper_length (predictor) on x-axis, body_mass (response) on y-axis
penguins %>% 
  ggplot(aes(flipper_length_mm, body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm")

# Add sex to the model:
num_model2 <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins)
get_regression_summaries(num_model2)
get_regression_table(num_model2)
get_regression_points(num_model2)

# Visualize with separate lines:
# Model: body_mass_g ~ flipper_length_mm + sex
# Flipper_length (predictor) on x-axis, body_mass (response) on y-axis
penguins %>% drop_na() %>% 
  ggplot(aes(flipper_length_mm, body_mass_g, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 

# Add species to the model:
num_model3 <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
get_regression_summaries(num_model3)
get_regression_table(num_model3)
get_regression_points(num_model3)

# Visualize:
# Model: body_mass_g ~ flipper_length_mm + species
# Flipper_length (predictor) on x-axis, body_mass (response) on y-axis
penguins %>% drop_na() %>% 
  ggplot(aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 

# Full model with all predictors:
num_model4 <- lm(body_mass_g ~ flipper_length_mm + species + sex, data = penguins)
get_regression_summaries(num_model4)
get_regression_table(num_model4)
get_regression_points(num_model4)

# -----------------------------------------------------------------------------

## Simpson's Paradox: When everything reverses!

# Remember that negative correlation between bill length and bill depth?
# Let's investigate it more carefully...

# Model WITHOUT species:
num_model_A <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
get_regression_summaries(num_model_A)
get_regression_table(num_model_A)
get_regression_points(num_model_A)

# Visualize - notice the NEGATIVE slope:
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# Now add species to the model:
num_model_B <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)
get_regression_summaries(num_model_B)
get_regression_table(num_model_B)
get_regression_points(num_model_B)

# Visualize by species - now the slopes are POSITIVE!
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme(legend.position = "none")

# TASK 3: Discuss with a partner:
# - Why did the relationship reverse direction?
# - What does this tell us about the importance of considering confounding variables?
# - Can you think of other situations where Simpson's Paradox might occur?

### This is Simpson's Paradox!
# The overall trend is NEGATIVE, but within each species it's POSITIVE.
# This happens because species is a confounding variable - it affects both 
# bill length and bill depth. When we don't control for species, we get 
# a misleading conclusion.

# KEY LESSON: Always think carefully about what variables might be confounding
# your analysis. What looks like a relationship might disappear (or reverse!) 
# when you account for other factors.
