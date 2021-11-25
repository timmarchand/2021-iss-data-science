### Learn more about penguins!


## From the variable associations code
pacman::p_load(tidyverse, ggpubr, vcd, broom, moderndive, sjPlot, gapminder, rstatix)


### Because you need more penguins in your life
pacman::p_load(palmerpenguins)

library(palmerpenguins)

### Use glimpse() to find out about the dataset
glimpse(penguins)

# categorical
## nominal = species / island 
## binary = sex

# continuous
## integer = flipper_length_mm / body_mass_g / year
## ratio = bill_length_mm / bill_depth_mm

### Use summary() to get the statistical summary
summary(penguins)
## Three years of data?
## almost 50 / 50 for sex
## normal dist for coninuous ??
## fewer Chinstrap penguins and Penguins from Torgersen

## Plotting 

### univariate data (one variable)
### Categorical counts (bar or column charts -  - which ggplot2 geom should you use?)

penguins %>% count(species)
penguins %>% count(island)
penguins %>% count(sex)

## Can you visualise with histogram?

penguins %>% 
  ggplot(aes(species)) +
  geom_histogram()


## Only if you add an extra argument!
penguins %>% 
  ggplot(aes(species)) +
  geom_histogram(stat = "count")

## Alternatively, use geom_bar
penguins %>% 
  ggplot(aes(island)) +
  geom_bar()


## Or geom_col with count data (flexiblity to switch direction)
penguins %>% count(sex) %>% 
  ggplot(aes(n, sex)) +
  geom_col()

### Continuous distributions (histograms, density plots - which ggplot2 geom should you use?)
penguins %>% 
  ggplot(aes(bill_length_mm)) +
  geom_histogram()

penguins %>% 
  ggplot(aes(bill_depth_mm, fill = species)) +
  geom_histogram() +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_histogram() +
  facet_wrap(~species) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))


### bivariate (two variables)
###continuous vs continuous (scatter plots - which ggplot2 geom should you use?)
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() 

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")


penguins %>% 
  ggplot(aes(bill_length_mm, body_mass_g, color = species, shape = species)) +
  geom_point() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 
  


penguins %>% 
  ggplot(aes(body_mass_g, flipper_length_mm, color = species, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4"))
  

### categorical vs continuous (box-plots or faceted histograms)

penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_histogram() +
  facet_wrap(~species) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

penguins %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_boxplot()  +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

penguins %>% 
  ggplot(aes(flipper_length_mm, species)) +
  geom_jitter(aes( color = species, shape = species))  +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

penguins %>% 
  ggplot(aes(body_mass_g, island)) +
  geom_boxplot() +
  geom_jitter(aes(color = species, shape = species))  +
  scale_color_manual(values = c("darkorange","purple","cyan4"))


### categorical vs categorical (mosaic plots)

mosaic(~ species + island, data = penguins)

mosaic(~ species + island, data = penguins, shade = TRUE)

mosaic(~ sex + island, data = penguins, shade = TRUE)

mosaic(~ species + island + sex, data = penguins, shade = TRUE)


## Statistical tests

### Correlations - which type of bivariates can you test?

penguins %>% 
  rstatix::cor_test(body_mass_g, flipper_length_mm)

penguins %>% 
  rstatix::cor_test(bill_length_mm, bill_depth_mm)

### Chi-square test - which type of bivariates can you test?

penguins %>% 
  infer::chisq_test(island ~ sex)

penguins %>% 
  infer::chisq_test(island ~ species)

## Modelling

### Linear models for a selection of variables

## categorical variables

cat_model <- lm(body_mass_g ~ species, data = penguins)

get_regression_summaries(cat_model)
get_regression_table(cat_model)
get_regression_points(cat_model)


cat_model2 <- lm(body_mass_g ~ species + sex, data = penguins)

get_regression_summaries(cat_model2)
get_regression_table(cat_model2)
get_regression_points(cat_model2)

## continuous variables

num_model <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
get_regression_summaries(num_model)
get_regression_table(num_model)
get_regression_points(num_model)

penguins %>% 
  ggplot(aes(body_mass_g, flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

num_model2 <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins)
get_regression_summaries(num_model2)
get_regression_table(num_model2)
get_regression_points(num_model2)

penguins %>% drop_na() %>% 
  ggplot(aes(body_mass_g, flipper_length_mm, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 
  

num_model3 <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
get_regression_summaries(num_model3)
get_regression_table(num_model3)
get_regression_points(num_model3)

penguins %>% drop_na() %>% 
  ggplot(aes(body_mass_g, flipper_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) 
  

num_model4 <- lm(body_mass_g ~ flipper_length_mm + species + sex, data = penguins)
get_regression_summaries(num_model4)
get_regression_table(num_model4)
get_regression_points(num_model4)


num_model_A <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
get_regression_summaries(num_model_A)
get_regression_table(num_model_A)
get_regression_points(num_model_A)

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

num_model_B <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)
get_regression_summaries(num_model_B)
get_regression_table(num_model_B)
get_regression_points(num_model_B)

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme(legend.position = "none")


### We have an example of Simpsons's Paradox!!

