 s## Is there a significant difference in the average ratings by season? (Think of season as categorical data). 

library(tidyverse)
library(moderndive)
library(broom)
library(ggpubr)
library(effects)

# reload data
GoT <- read_csv("https://tinyurl.com/iss-GoT-web")


## statistical testing ----

GoT_model <- lm(rating ~ season, data = GoT)
summary(GoT_model)

get_regression_summaries(GoT_model)
## r_squared = correlation coefficient
## adj_r_squared = adjusted r squared, adjusted for multiople variables
## statistic = F-statistic
## nobs = number of observations

get_regression_table(GoT_model)
## (Intercept) estimate = predicted value for the base category (i.e mean overall for each season)
## season1 etc estimate = predicted change in value for going from mean overall to that season
## statistic = t value
## lower_ci = lower confidence interval
## upper_ci = upper confidence interval

### determine the nature of the effect(s) graphically
## With an effects plot
library(effects)
season <- effect("season", GoT_model)
plot(season, ylim=range(GoT$rating), xlab=" Rating-Effects plot for each season of Game of Thrones", grid=TRUE)

## With a boxplot and stat_compare_means
ggplot(GoT, aes(x = season, y = rating)) +
  geom_boxplot() +
  labs(x = "Season", y = "IMDb Rating",
       title = "Game of Throne Ratings by season") +
  ggpubr::stat_compare_means(method = "anova")

## Do people tend to vote on episodes they like? (Is there a correlation between number of votes and rating scores?) ----


## statistical test
res <- cor.test(GoT$votes,GoT$rating, method = "spearman")
broom::tidy(res)


## visualise
GoT %>% 
  ggplot(aes(x = rating, y = votes)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(method="spearman") +
  theme_minimal()


# check the distributions ----

ggplot(GoT) +
  aes(x = rating) +
  geom_histogram() +
  theme_minimal()

ggplot(GoT) +
  aes(x = votes) +
  geom_histogram() +
  theme_minimal()


x <- GoT$rating
GoT$rating_trans <- log10(max(x+1) - x)

x <- GoT$votes
GoT$votes_trans <- log10(x)

ggplot(GoT) +
  aes(x = rating_trans) +
  geom_histogram() +
  theme_minimal() # slightly better

ggplot(GoT) +
  aes(x = votes_trans) +
  geom_histogram() +
  theme_minimal() # not much improved

## let's visualise
GoT %>% 
  ggplot(aes(x = rating_trans, y = votes)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(method="spearman") +
  theme_minimal()

# opposite result!


## let's make  two models - one with rating transformed ----

# model 1
GoT_model1 <- lm(votes ~ rating, data = GoT)

# details
summary(GoT_model1)
get_regression_summaries(GoT_model1) # r-squared 0.29
get_regression_table(GoT_model1)
get_regression_points(GoT_model1) 

# model 2
GoT_model2 <- lm(votes ~ rating_trans, data = GoT)

# details
summary(GoT_model2)
get_regression_summaries(GoT_model2) # r-squared 0.096
get_regression_table(GoT_model2)
get_regression_points(GoT_model2) 

# model 1 actually fits better

## Visualise with an effects plot ----
library(effects)

vote <- effect("rating", GoT_model1)
plot(vote, ylim=range(GoT$votes), xlab=" Vote-Effects plot for ratings of Game of Thrones", grid=TRUE)

### But what if there is an interaction effect? ---

## What is an interaction effect?

condiments <- read_csv("https://tinyurl.com/iss-condiments-web")

sauce_mdl <- lm(enjoyment ~ food*sauce, condiments)

get_regression_summaries(sauce_mdl)
get_regression_table(sauce_mdl)

ggplot(condiments, aes(x = food, y = enjoyment, colour = sauce)) +
  geom_boxplot() +
  labs(x = "Sauce", y = "Enjoyment", color = "Food") +
  theme_minimal()

sauce <- effects::effect("food*sauce", sauce_mdl)
plot(sauce, ylim=range(condiments$enjoyment), xlab="Interaction Effects plot of sauce and food on enjoyment", grid=TRUE)


## Back to GoT, Make a new model for "rating*season" this adds interaction to the model ---

# Fit regression model:
GoT_model3 <- lm(votes ~ rating * season, data = GoT)

# Get regression table:
get_regression_summaries(GoT_model3)
get_regression_table(GoT_model3)


## Let's visualise the interaction effect

p1 <- ggplot(GoT, aes(x = rating, y = votes, color = season)) +
  geom_point() +
  labs(x = "Rating", y = "Votes", color = "Season") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
p1
# Let's visualise without the interaction effect using geom_parallel_slopes

p2 <- ggplot(GoT, aes(x = rating, y = votes, color = season)) +
  geom_point() +
  labs(x = "Rating", y = "Votes", color = "Season") +
  geom_parallel_slopes( se = FALSE) +
  theme_minimal()

gridExtra::grid.arrange(p1,p2,nrow=1)

## The parallel slopes model shows all the lines with different intercepts, but all the slopes are of the same positive nature.
## Depending on the data, parallel slopes may fit the real values close enough to be preferable to a model with interaction
## Using Occam's razor - simple is best (https://en.wikipedia.org/wiki/Occam%27s_razor)

## Out of interest, we can fit the regression model without an interaction 
## by using '+' in the equation rather than '*'
# Fit regression model:
score_model_parallel_slopes <- lm(votes ~ rating + season, data = GoT)
# Get regression table:
get_regression_table(score_model_parallel_slopes)

