## Associations between variables set up ----

## OPTIONAL clear memory
 rm(list=ls(all=TRUE))

## load libraries
# library(tidyverse)
# library(ggpubr) # for adding stats to plots
# library(infer) # for stats tests on tidy data
# library(vcd) # for chi-squared and mosaic plot
# library(broom) # for creating tidy tables from regression models
# library(moderndive) # more easy to read tables from regression models
# library(effects) # ADVANCED: handling of effects
# library(sjPlot) # ADVANCED: for plotting of effects
# library(gapminder) # for data
# library(janitor) # for tabyl() function

## Efficeint alternative - use pacman!
pacman::p_load(tidyverse, ggpubr, infer, vcd, broom, moderndive, effects, sjPlot, gapminder, janitor)

## load data from data folders ----

dat <- read_csv("data/africa_guess.csv")

## If this doesn't work
## dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQul_g9GtsSj-30sQ2hOE_rixxy8gH6AdhawcT5AoRJodJdGZnBgqtwgDAmfab0CJ1AjmFCrKmVw5MA/pub?gid=1799592268&single=true&output=csv")

# get rid of outlier
df <- dat %>% filter(height>7)

## for some real world examples

## get the data from online csv
resume <- read_csv("https://tinyurl.com/iss-racial-disc-web")
## save it into your data folder
write_csv(resume,"data/resume.csv")

## next time, you can load the data directly here
resume <- read_csv("data/resume.csv")

## get gapminder data
gapminder <-  gapminder::gapminder

## continuous vs categorical:  africa-n_africa data----
## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)


# check the data
glimpse(df)

## Try the boxplot again
ggplot(df) +
  aes(x = "", y = n_africa, fill = birthday) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  theme_minimal() +
  stat_compare_means(method = "t.test")
## Still potentially different somehow...

## Density plot again
ggplot(df) +
  aes(x = n_africa,fill = birthday) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=birthday)) +
  labs(x = "Guess for how many countries in Africa", y = "Probability Density", title = "Are guesses related to birthday dates?", fill = "birthday date") +
  theme_minimal()

## Do these plots suggest a difference in the groups?
## Are the differences significant?

## use the t.test or anova functions

## t.test {Base R} ----
## requires us to work on vectors

## but first we need to split the df into two groups using dplyr
df_odd <- df %>% filter(birthday == "odd")
df_even <- df %>% filter(birthday == "even")

## Extract vectors from dataframes with $
## Try the t.test function
t.test(df_even$n_africa,df_odd$n_africa)
## as we saw before p-value very small!!
## this gives us the mean values of our two groups (mean of x vs mean of y)
## the 95% confidence intervals shows what the difference could be in 95% of similar samples


## You can feed the result into tidy() to get a table of the results
t.test(df_even$n_africa,df_odd$n_africa) %>% broom::tidy()

## estimate = difference in means
## estimate 1 = mean of group 1
## estimate 2 = mean of group 2
## statistic = t score
## parameter = degrees of freedom

## t_test using {infer} on the dataframe ----
## The infer package allows us to work on the dataframe directly

df %>% 
  infer::t_test(formula = n_africa ~ birthday)

## statistic = t score
## t_df = degrees of freedom
## alternative = hypothesis chosen (Options are "two-sided" (default), "greater", or "less")
## lower_ci = lower confidence interval
## upper_ci = upper confidence interval

##  t-test from regression ----

## Using regression: any t-test or anova result can be found with a linear regression model
## use the function lm(dependent/outcome variable ~ independent/predictor, data = df)
cat_model <- lm(n_africa ~ birthday, data = df)
summary(cat_model)
## (Intercept) estimate = predicted value for the base category (i.e mean for even guess of n_africa)
## birthdayodd estimate = predicted change in value for going from even to odd
## Pr(>|t|) = probability, or p-value
## Multiple R-squared = correlation coefficient
## Adjusted R-squared = adjusted in case of multiple variables

## From moderndive:

get_regression_summaries(cat_model)
## r_squared = correlation coefficient
## adj_r_squared = adjusted r squared, adjusted for multiople variables
## statistic = F-statistic
## nobs = number of observations

get_regression_table(cat_model)
## (Intercept) estimate = predicted value for the base category (i.e mean for even n_africa)
## numberodd estimate = predicted change in value for going from even to odd
## statistic = t value
## lower_ci = lower confidence interval
## upper_ci = upper confidence interval

regression_points <- get_regression_points(cat_model)
regression_points
## n_africa_hat shows the predicted values for the model (the means)
## residuals shows the difference between the fitted (predicted) values and the real (n_africa) values


### ADVANCED STUFF - plotting effects of a linear model with a binary predictor ----
## (OKAY TO SKIP, or paste for your own project)
## EFFECTS PLOTS show you the nature of the statistical test
## Replace the following variables for your own data:
## n_africa
## number
## df
## num (make your own abbreviated name)


## using {sjPlot}
## assign the model to model.01

model.01 <- cat_model
set_theme(base = theme_sjplot())
plot_model(model.01, type = "pred", , terms = c("birthday"))

## using {effects} package

### determine the nature of the effect(s) numerically

# returns the estimates and 95% confidence interval values
cbind(coef(model.01), confint(model.01)) 
(preds.hyp <- data.frame(num <- effect("birthday", model.01)))

## check which parts match the get_regression_table output!

# get the 'predictions' of the model for the current data using predict
head(predict(model.01)) 
## check which parts match the get_regression_points output!

### determine the nature of the effect(s) graphically
plot(num, ylim=range(df$n_africa), xlab="Birthday date even_odd", grid=TRUE)     # prediction


## continuous vs categorical:  real world data----
## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)

## Examine whether there is a difference associated between continent and life expectancy in 2007

## filter for 2007
gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)

## take a look at the data
glimpse(gapminder2007)

## make a faceted histogram plot by continent
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy",
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~ continent, nrow = 2)

## make a boxplot plot of lifeExp  by continent
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")


## as with t-test, can add Anova stat to the plot
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent") +
  stat_compare_means(method = "anova")

## use dplyr to get a table of means and medians
lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp),
            mean = mean(lifeExp))

lifeExp_by_continent

## Regression modelling also possible for multiple categories
lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
summary(lifeExp_model)
get_regression_summaries(lifeExp_model)
get_regression_table(lifeExp_model)
get_regression_points(lifeExp_model, ID = "country")


### ADVANCED - plotting effects of a linear model with a categorical predictors ----
## (OKAY TO SKIP, or paste for your own project)
## EFFECTS PLOTS show you the nature of the statistical test
## variable names have been replaced

set_theme(base = theme_light())
plot_model(lifeExp_model, type = "pred", terms = c("continent")) +
  theme_sjplot()


## Base R and {effects} alternative
## Alternative to get_regression_table(lifeExp_model)
(preds.hyp <- data.frame(num <- effect("continent", lifeExp_model)))

# get the 'predictions' of the model for the current data using predict
## Alternative to get_regression_points(lifeExp_model)
gapminder2007 %>% 
  mutate(lifeExp_hat = predict(lifeExp_model),
         residual = lifeExp - lifeExp_hat)
 
### determine the nature of the effect(s) graphically
## Alternative to plot_model
## Get effect details
num <- effect("continent", lifeExp_model)
## Plot in Base R
plot(num,  xlab="Continent", grid=TRUE)     # prediction


## categorical vs categorical ----
## compare joint frequencies in contingencies tables
## statistical test from the Chi-squared family
## plot joint frequencies with bar plots or mosaic plots

## Use {Base R} table()
attach(df)
table(birthday)
table(visit)
table(birthday,visit)

# get a table of the proportions
prop.table(table(visit)) # values between 0 ~ 1
# multiply by 100 for percentages
prop.table(table(birthday))*100
# use round for nicer numbers
round(prop.table(table(birthday))*100)

## to get a relative frequency table, use the margin argument
# margin = 2 selects the columns, so now the columns add up to 100%
round(prop.table(table(birthday,visit),margin = 2)*100)
# margin = 1 selects the rows, so now the rows add up to 100%
round(prop.table(table(birthday,visit),margin = 1)*100)

## contingency tables, put two vectors together
table(birthday, visit) # first value for rows, second value for columns

## easily get the chi-squared value using summary()
summary(table(birthday, visit))
# p-value > 0.9 - of course, no significant difference
# Chi-squared approximation may be incorrect message - because 'yes' column very small

# use fisher exact test as alternative
fisher.test(birthday,visit) # p-value = 1!!
detach(df)

## Alternative to tables with {dplyr} and {janitor} ----
## Use count to create a tidy dataframe
df %>% count(birthday)
df %>% count(visit)
df %>% count(birthday,visit)
## Add proportions
df %>% count(birthday, visit) %>%  
  mutate(prop = prop.table(n))

## Alternative with {janitor}
df %>% tabyl(birthday,visit) %>% adorn_percentages()

## Chisq_test with {infer} ----
df %>% chisq_test(formula = birthday ~ visit)


## plotting bar chart with ggplot
ggplot(df) +
  aes(x = visit, fill = birthday) +
  geom_bar() +
 # scale_fill_viridis_d(option = "cividis") + # for a fancy colour scheme
  coord_flip() + # change x and y axis
  theme_minimal()

## plotting with a mosaic plot from vcd package
## some useful stats from assocstats() function
attach(df)
assocstats(table(birthday, visit))
# Pearson value same as the p_value above

##the mosaic plot
mosaic(~ birthday + visit,
       direction = c("v", "h"),
       data = df,
       shade = TRUE)

detach(df)

## Real world example - racial discrimination in the labour market ----
# Does racial discrimination exist in the labor market? Or, should racial disparities
# in the unemployment rate be attributed to other factors such as racial gaps in
# educational attainment? To answer this question, two social scientists conducted
# the following experiment (Bertrand & Mullainathan, 2004)**. In response to newspaper ads, the researchers sent out
# résumés of fictitious job candidates to potential employers. They varied only the names
# of job applicants, while leaving the other information in the résumés unchanged.

# **Bertrand, M., & Mullainathan, S. (2004).
# Are Emily and Greg more employable than Lakisha and Jamal?
# A field experiment on labor market discrimination.
# American economic review, 94(4), 991-1013.

# https://www.aeaweb.org/articles?id=10.1257/0002828042002561



glimpse(resume)


### Base R and table()

## as before, you can get a contingency table with table and two column vectors
(table(resume$race,resume$call))
## or a table of percentages, rounded down
round(prop.table(table(resume$race,resume$call))*100,1) # rounded to one decimal place


## easily get the chi-squared value using summary()
summary((table(resume$race,resume$call)))# p-value < 0.001


### Tidy alternative with {infer}

resume %>% mutate(call = factor(call)) %>% # change call to a factor (categorical) variable
 # mutate(call = factor(call)) %>%   ##  the response variable is expected to be categorical error
  infer::chisq_test(call ~ race)


## plotting bar chart with ggplot
resume %>%  
  ggplot(aes(x = race, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") +
  coord_flip() +
  theme_minimal()

## Not the best plot ever!

## check the problem with the variables
class(resume$race)
class(resume$call)
## call is saved as a numeric variable!

## change to logical and try again
## plotting bar chart with ggplot
resume %>% mutate(call = factor(call)) %>% 
  ggplot(aes(x = race, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") +
  coord_flip() +
  theme_minimal()



## plotting with a mosaic plot from vcd package

assocstats(table(resume$race, resume$call))
# Pearson value same as the p=value above

mosaic(~ race + call,
       direction = c("v", "h"),
       data = resume,
       shade = TRUE)

## this mosaic plot is now shaded to show which group
## is significantly over-represented (white) and
## which group is significantly under-represented (black)



## DIY: use dplyr verbs to create a new df with new columns: ----

## black_male, black_female, white_male and white_female applicants
## find out if there is evidence for racial discrimination  for both sexes

## create res_2 variable by
## taking resume df
## group by race and sex variables
## mutate a new column called applicant using paste() to combine the character strings
## ungroup
## select only new applicant and call variables


## continuous vs continuous  africa-guess data ----
## compare values with scatter plots
## assess association with regression lines
## statistical tests from the R-squared family (or the linear model of regression)

## Let's revisit the df example of height X n_africa
## we saw the following code when we looked at in week_5_hw_plots:

ggplot(df) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method = "lm")  +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## simple correlations without plotting
## working on vectors
attach(df)
## this give the the R value, or slope of the line
cor(x=height, y=n_africa)
## you can choose a method depending on the nature of the data
cor(x = height, y = n_africa, method = "spearman")

## to get other statistics, you can use cor.test
cor.test(height,n_africa, method = "pearson")

## if you save the result as an object...
res <- cor.test(height,n_africa, method = "pearson")

## you can produce a nice table using the broom::tidy function
broom::tidy(res)
## estimate = correlation
## statistic = t score
## parameter = degrees of freedom
## conf.low = lower 95% confidence level
## conf.high = upper 95% confidence level

## in this case the confidence levels go from -0.XXX to 0.XXX
## if it crosses over 0, there's a good chance there is no relationship at all
## as evidenced by the high p.value

## note that for many correlation tests, the variables are assumed to be distributed normally
## always good practice to check that first
detach(df)



## QUESTION - how can you check variables have normal distribution? ----


## linear regression models for two continuous variables ----

cont_model <- lm(formula = n_africa ~ height, data = df)
summary(cont_model)
get_regression_summaries(cont_model)
get_regression_table(cont_model)
get_regression_points(cont_model)


## Our original plot, but now with expanded x and y axes
ggplot(df) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method = "lm")  +
  xlim(0,200) +
  ylim(-20,100) +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## another plot, this time with geom_abline added
## using the intercept value as the estimate for Intercept
## and the the slope value as the estimate for height

ggplot(df) +
  aes(x = height, y = n_africa) +
  geom_point() +
  geom_smooth(method = "lm")  +
 geom_abline(mapping=aes(slope=0.359, intercept=-19.5)) +
  xlim(0,200) +
  ylim(-20,100) +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## continuous vs continuous  real world example ----
## compare values with scatter plots
## assess association with regression lines
## statistical tests from the R-squared family (or the linear model of regression)

## simple correlations without plotting
## attach to access the column names
attach(gapminder)
## this give the the R value, or slope of the line
cor(x=gdpPercap, y=lifeExp)

## you can choose a method depending on the nature of the data
cor(x=gdpPercap, y=lifeExp, method = "kendall")

## to get other statistics, you can use cor.test
cor.test(gdpPercap,lifeExp, method = "pearson")

## if you save the result as an object...
res <- cor.test(gdpPercap,lifeExp, method = "pearson")

## you can produce a nice table using the broom::tidy function
broom::tidy(res)

## detach after finishing
detach(gapminder)

## Plotting the two continuous variables
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(method="pearson") +
theme_minimal()

## Does the line look like a good fit??

# check the distributions of gdpPercap and lifeExp

ggplot(gapminder) +
  aes(x = lifeExp) +
  geom_histogram() +
  theme_minimal()

ggplot(gapminder) +
  aes(x = gdpPercap) +
  geom_histogram() +
  theme_minimal()

## gdpPercap very far from normal
## try some kind of transformation

# log transform gdpPercap
ggplot(gapminder) +
  aes(x = log2(gdpPercap)) +
  geom_histogram() +
  theme_minimal()

# distribution looks better

gapminder %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()
## much nicer regression line and bigger R value! (0.81 vs 0.58)



## let's make two models - one without log transformed variables

gdp_life_model01 <- lm(lifeExp ~ gdpPercap, data = gapminder)

# details
summary(gdp_life_model01)
get_regression_summaries(gdp_life_model01)
get_regression_table(gdp_life_model01)
get_regression_points(gdp_life_model01)

## second one with log transformation
## first do that using dplyr

gapminder_log <- gapminder %>%
  mutate(log_gdp = log(gdpPercap))

names(gapminder_log)

## fit the model
gdp_life_model02 <- lm(lifeExp ~ log_gdp, data = gapminder_log)

# details
summary(gdp_life_model02)
get_regression_summaries(gdp_life_model02)
get_regression_table(gdp_life_model02)


regression_points <- get_regression_points(gdp_life_model02)
regression_points



